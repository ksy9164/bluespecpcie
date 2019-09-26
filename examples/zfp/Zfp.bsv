import Connectable::*;
import FIFO::*;
import Vector::*;
interface ZfpIfc;
    method Action put(Vector#(4, Bit#(64)) data);
    method Action put_encoding_size(Bit#(5) size);
    method Action put_matrix_cnt(Bit#(32) cnt);
    method ActionValue#(Bit#(328)) get_last;
    method ActionValue#(Bit#(164)) get;
endinterface

/* function */
function Bit#(13) get_max(Bit#(13) a, Bit#(13) b, Bit#(13) c, Bit#(13) d);
    if (a >= b && a >= c && a >= d)
        return a;
    else if (b >= a && b >= c && b >= d)
        return b;
    else if (c >= a && c >= b && c >= d)
        return c;
    else
        return d;
endfunction
function Bit#(32) intShift(Bit#(32) t);
    Bit#(1) s;
    s = t[31];
    t = t >> 1;
    t[31] = s;
    return t;
endfunction
function Bit#(8) permutation_idx(Bit#(8) idx);
    Bit#(8) perm_idx = 0;
    case (idx)
       0 : perm_idx = 0;
       1 : perm_idx = 1;
       2 : perm_idx = 4;
       3 : perm_idx = 8;
       4 : perm_idx = 2;
       5 : perm_idx = 3;
       6 : perm_idx = 6;
       7 : perm_idx = 11;
       8 : perm_idx = 5;
       9 : perm_idx = 7;
       10 : perm_idx = 10;
       11 : perm_idx = 13;
       12 : perm_idx = 9;
       13 : perm_idx = 12;
       14 : perm_idx = 14;
       15 : perm_idx = 15;
    endcase
    return perm_idx;
endfunction
function Bit#(3) get_table_idx (Bit#(16) d);
    Bit#(5) msb = 0;
    if (d >= 256) begin
        if (d >= 4096) begin
            for (Bit#(5) j = 12; j < 16; j = j+1) begin
                if (d[j] == 1) begin
                    msb = j;
                end
            end   
        end else begin
            for (Bit#(5) j = 8; j < 12; j = j+1) begin
                if (d[j] == 1) begin
                    msb = j;
                end
            end   
        end
    end else begin
        if (d >= 16) begin
            for (Bit#(5) j = 4; j < 8; j = j+1) begin
                if (d[j] == 1) begin
                    msb = j;
                end
            end   
        end else begin
            for (Bit#(5) j = 0; j < 4; j = j+1) begin
                if (d[j] == 1) begin
                    msb = j;
                end
            end   
        end
    end
    if (msb == 0)
        return 0;
    else if (msb == 1)
        return 1;
    else if (msb > 1 && msb < 4)
        return 2;
    else if (msb >= 4 && msb < 8)
        return 3;
    else
        return 4;
endfunction
function Bit#(16) get_each_data (Vector#(16,Bit#(8)) in, Bit#(8) num);
    Bit#(16) acuBits;
    for (Bit#(8) i = 0; i < 16; i = i + 1) begin
        acuBits[i] = in[i][num];
    end
    return acuBits;
endfunction
function Bit#(32) int_to_uint(Bit#(32) t);
  return (t + 32'haaaaaaaa) ^ 32'haaaaaaaa;
endfunction
function Bit#(4) get_h_amount(Bit#(3) h);
    if (h == 0)
        return 1;
    else
        return 3;
endfunction
function Bit#(7) get_d_amount(Bit#(3) h);
    Bit#(7) amount = 0;
    case(h)
        0:  amount = 1;
        4:  amount = 1;
        5:  amount = 4;
        6:  amount = 8;
        7:  amount = 16;
    endcase
    return amount;
endfunction
(* synthesize *)
module mkZfp (ZfpIfc);
    /* Rule to Rule FIFO */
    FIFO#(Vector#(4, Bit#(64))) inputQ <- mkFIFO;
    FIFO#(Bit#(164)) outputQ <- mkFIFO;
    FIFO#(Vector#(4, Bit#(64))) toGetFraction <- mkFIFO;
    FIFO#(Vector#(4, Bit#(32))) toMakeFixedPoint <- mkSizedFIFO(5);
    FIFO#(Bit#(13)) sendMaximumExp <- mkSizedFIFO(5);
    FIFO#(Vector#(4, Bit#(7))) shiftQ <- mkSizedFIFO(5);
    
    FIFO#(Vector#(4, Bit#(32))) toRowBlockTransform <- mkFIFO;
    FIFO#(Vector#(4, Bit#(32))) toRowBlockTransform2Q <- mkFIFO;
    FIFO#(Vector#(4, Bit#(32))) toRowBlockTransform3Q <- mkFIFO;

    Vector#(16,FIFO#(Bit#(32))) toColBlockTransform <- replicateM(mkFIFO);
    Vector#(16,FIFO#(Bit#(32))) toColBlockTransform2Q <- replicateM(mkFIFO);
    Vector#(16,FIFO#(Bit#(32))) toColBlockTransform3Q <- replicateM(mkFIFO);

    Vector#(16,FIFO#(Bit#(32))) toPermutation <- replicateM(mkFIFO);
    Vector#(16,FIFO#(Bit#(32))) toDivideBits <- replicateM(mkFIFO);
    Vector#(16,Vector#(4,FIFO#(Bit#(8)))) toGatherBits <- replicateM(replicateM(mkFIFO));
    Vector#(32,FIFO#(Bit#(16))) toMakeHeader <- replicateM(mkFIFO);
    Vector#(32,FIFO#(Bit#(16))) merge_step1_data<- replicateM(mkFIFO);
    Vector#(32,FIFO#(Bit#(3))) merge_step1_header<- replicateM(mkFIFO);

    Vector#(8,FIFO#(Bit#(64))) merge_step2_data <- replicateM(mkFIFO);
    Vector#(8,FIFO#(Bit#(12))) merge_step2_header<- replicateM(mkFIFO);
    Vector#(8,FIFO#(Bit#(7))) merge_step2_data_amount<- replicateM(mkFIFO);
    Vector#(8,FIFO#(Bit#(4))) merge_step2_header_amount<- replicateM(mkFIFO);

    Vector#(4,FIFO#(Bit#(128))) toOut_d<- replicateM(mkFIFO);
    Vector#(4,FIFO#(Bit#(24))) toOut_h<- replicateM(mkFIFO);
    Vector#(4,FIFO#(Bit#(8))) toOut_d_amount<- replicateM(mkFIFO);
    Vector#(4,FIFO#(Bit#(5))) toOut_h_amount<- replicateM(mkFIFO);

    FIFO#(Bit#(328)) lastOutputQ <- mkFIFO;

    /* Encoding Size */
    Reg#(Bit#(6)) encodeBudget <- mkReg(32);
    Reg#(Bit#(32)) totalMatrixCnt <- mkReg(100);

    /* Encode Map */
    Vector#(5,Reg#(Bit#(3))) msbCodeTable <- replicateM(mkReg(0));
    msbCodeTable[0] <- mkReg(0); msbCodeTable[1] <- mkReg(4); msbCodeTable[2] <- mkReg(5); msbCodeTable[3] <- mkReg(6); msbCodeTable[4] <- mkReg(7);

    Reg#(Bit#(2)) inputCycle <- mkReg(0);
    /* Exp FIFO */
    FIFO#(Vector#(4, Bit#(13))) exp <- mkSizedFIFO(5);
    FIFO#(Bit#(13)) maximumExp <- mkSizedFIFO(5);
    FIFO#(Bit#(13)) encodingExp <- mkSizedFIFO(31);
    Reg#(Bit#(13)) currentExpMax <- mkReg(0);
    Reg#(Bit#(13)) expMax <- mkReg(0);
    Reg#(Bit#(13)) sendExpMax <- mkReg(0);
    Reg#(Bit#(32)) outBuffCycle <- mkReg(0);

    /* BlockTransform */
    Reg#(Bit#(2)) transCycle <- mkReg(0);

    Reg#(Bit#(32)) cntSequence <- mkReg(0);

    /* output */
    Reg#(Bit#(2)) outCycle <- mkReg(0);
    Reg#(Bit#(328)) output_buffer <- mkReg(0);
    Reg#(Bit#(9)) output_buffer_off <- mkReg(0);

    rule getMaxExp;
        inputQ.deq;
        Vector#(4, Bit#(64)) in = inputQ.first;
        Bit#(13) tempExpMax = 0;

        /* Get 256bit data & Calculate current Maximum Exp in this vector
         * Update ExpMax & Dequeue to Next Step (getFraction) */
        Vector#(4, Bit#(13)) matrixExp = replicate(0);
        for (Integer i = 0; i < 4; i = i+1) begin
            matrixExp[i] = truncate(in[i]>>52);
        end
        tempExpMax = get_max(matrixExp[0],matrixExp[1],matrixExp[2],matrixExp[3]);
        
        /* Is that last? */
        if (inputCycle == 3) begin
            if (currentExpMax > tempExpMax) begin
                maximumExp.enq(currentExpMax);
                encodingExp.enq(currentExpMax);
                currentExpMax <= 0;
                expMax <= currentExpMax;
            end else begin
                maximumExp.enq(tempExpMax);
                encodingExp.enq(tempExpMax);
                expMax <= tempExpMax;
                currentExpMax <= 0;
            end
        end
        /* Not last */
        else begin
            if (currentExpMax < tempExpMax) begin
                currentExpMax <= tempExpMax;
            end
        end
        inputCycle <= inputCycle + 1;
        exp.enq(matrixExp);
        toGetFraction.enq(in);
    endrule

    rule getFraction;
        toGetFraction.deq;
        Vector#(4, Bit#(64)) in = toGetFraction.first;
        Vector#(4, Bit#(32)) outd = replicate(0);
        Vector#(4, Bit#(52)) frac = replicate(0);

        /* Get Fraction from double data be using Bit operation <<, zeroextention, truncate
            * Make output vecotor and send to NextStep which is makeFixedPoint */
        for (Integer i = 0; i < 4; i = i+1) begin
            frac[i] = truncate(in[i]);
            /* Make Signed Extention */
            frac[i] = frac[i] >> 1;
            frac[i][51] = 1;
            outd[i] = truncateLSB(frac[i]);
        end
        toMakeFixedPoint.enq(outd);
    endrule

    Reg#(Bit#(2)) sendExp_handle <- mkReg(0);
    rule sendExp;
        Bit#(13) tExpMax = 0;
        if (sendExp_handle == 0) begin
            maximumExp.deq; //Get Maximum EXP
            tExpMax = maximumExp.first;
            sendExpMax <= tExpMax;
        end else begin
            tExpMax = sendExpMax;
        end
        sendMaximumExp.enq(tExpMax);
        sendExp_handle <= sendExp_handle + 1;
    endrule

    rule calShift;
        sendMaximumExp.deq;
        exp.deq; // Get element's exp
        let tExpMax = sendMaximumExp.first;
        let expCurrent = exp.first;
        Vector#(4, Bit#(7)) outd = replicate(0);
        for (Integer i = 0; i < 4; i = i+1) begin
            Bit#(13) term = tExpMax - expCurrent[i] + 2;
            Bit#(7) shift = 0;
            if (term > 31) begin
                shift = 32;
            end else begin
                shift = truncate(term);
            end
            outd[i] = shift;
        end
        shiftQ.enq(outd);
    endrule

    rule makeFixedPoint;
        toMakeFixedPoint.deq; // Get (32x4)Bits fraction data
        shiftQ.deq;
        let in = toMakeFixedPoint.first;
        let shift = shiftQ.first;

        Vector#(4, Bit#(32)) outd = replicate(0);
        /* Make Fixed Point by considering maximum Exp in Matrix */
        for (Integer i = 0; i < 4; i = i+1) begin
            if (shift[i] > 31) begin
                outd[i] = 0;
            end else begin
                outd[i] = in[i] >> shift[i];
            end
        end
        toRowBlockTransform.enq(outd);
    endrule

    /* Row Transform */
    rule rowBlockTransform;
        toRowBlockTransform.deq;
        Vector#(4, Bit#(32)) in = toRowBlockTransform.first;
        in[0] = (in[0]+in[3]); in[0] = intShift(in[0]); in[3] = (in[3]-in[0]);
        in[2] = (in[2]+in[1]); in[2] = intShift(in[2]); in[1] = (in[1]-in[2]);
        toRowBlockTransform2Q.enq(in);
    endrule

    rule rowBlockTransform_2;
        toRowBlockTransform2Q.deq;
        Vector#(4, Bit#(32)) in = toRowBlockTransform2Q.first;
        in[0] = (in[0]+in[2]); in[0] = intShift(in[0]); in[2] = (in[2]-in[0]);
        in[3] = (in[3]+in[1]); in[3] = intShift(in[3]); in[1] = (in[1]-in[3]);
        toRowBlockTransform3Q.enq(in);
    endrule

    rule rowBlockTransform_3;
        Bit#(5) idx = zeroExtend(transCycle);
        toRowBlockTransform3Q.deq;
        Vector#(4, Bit#(32)) in = toRowBlockTransform3Q.first;
        in[3] = (in[3]+ intShift(in[1])); in[1] = (in[1] - (intShift(in[3])));
        toColBlockTransform[idx*4].enq(in[0]);
        toColBlockTransform[idx*4+1].enq(in[1]);
        toColBlockTransform[idx*4+2].enq(in[2]);
        toColBlockTransform[idx*4+3].enq(in[3]);
        transCycle <= transCycle + 1;
    endrule

    /* Colum Transform */
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule colTransform;
            Vector#(4, Bit#(32)) in = replicate(0);
            toColBlockTransform[i+12].deq;
            toColBlockTransform[i+8].deq;
            toColBlockTransform[i+4].deq;
            toColBlockTransform[i].deq;
            in[0] = toColBlockTransform[i].first;
            in[1] = toColBlockTransform[i+4].first;
            in[2] = toColBlockTransform[i+8].first;
            in[3] = toColBlockTransform[i+12].first;

            in[0] = (in[0]+in[3]); in[0] = intShift(in[0]); in[3] = (in[3]-in[0]);
            in[2] = (in[2]+in[1]); in[2] = intShift(in[2]); in[1] = (in[1]-in[2]);

            toColBlockTransform2Q[i].enq(in[0]);
            toColBlockTransform2Q[i+4].enq(in[1]);
            toColBlockTransform2Q[i+8].enq(in[2]);
            toColBlockTransform2Q[i+12].enq(in[3]);
        endrule
    end
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule colTransform_2;
            Vector#(4, Bit#(32)) in = replicate(0);
            toColBlockTransform2Q[i+12].deq;
            toColBlockTransform2Q[i+8].deq;
            toColBlockTransform2Q[i+4].deq;
            toColBlockTransform2Q[i].deq;
            in[0] = toColBlockTransform2Q[i].first;
            in[1] = toColBlockTransform2Q[i+4].first;
            in[2] = toColBlockTransform2Q[i+8].first;
            in[3] = toColBlockTransform2Q[i+12].first;

            in[0] = (in[0]+in[2]); in[0] = intShift(in[0]); in[2] = (in[2]-in[0]);
            in[3] = (in[3]+in[1]); in[3] = intShift(in[3]); in[1] = (in[1]-in[3]);

            toColBlockTransform3Q[i].enq(in[0]);
            toColBlockTransform3Q[i+4].enq(in[1]);
            toColBlockTransform3Q[i+8].enq(in[2]);
            toColBlockTransform3Q[i+12].enq(in[3]);
        endrule
    end
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule colTransform_2;
            Vector#(4, Bit#(32)) in = replicate(0);
            toColBlockTransform3Q[i+12].deq;
            toColBlockTransform3Q[i+8].deq;
            toColBlockTransform3Q[i+4].deq;
            toColBlockTransform3Q[i].deq;
            in[0] = toColBlockTransform3Q[i].first;
            in[1] = toColBlockTransform3Q[i+4].first;
            in[2] = toColBlockTransform3Q[i+8].first;
            in[3] = toColBlockTransform3Q[i+12].first;

            in[3] = (in[3]+ intShift(in[1])); in[1] = (in[1] - (intShift(in[3])));

            toPermutation[i].enq(in[0]);
            toPermutation[i+4].enq(in[1]);
            toPermutation[i+8].enq(in[2]);
            toPermutation[i+12].enq(in[3]);
        endrule
    end

    /* Permutation data */
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule permutation;
            for (Bit#(8)j=0; j<4; j=j+1) begin
                toPermutation[i+j*4].deq;
                let temp = toPermutation[i+j*4].first;
                temp = int_to_uint(temp);
                toDivideBits[permutation_idx(i+j*4)].enq(temp);
            end
        endrule
    end
    
    /* Extract Each 8Bits(total 64) from 4x4 Matrix */
    for (Bit#(8)i=0;i<16;i=i+1) begin
        rule extractBits;
            /* Get ith element */
            toDivideBits[i].deq;
            let in = toDivideBits[i].first;
            for (Bit#(8)j=0;j<4;j=j+1) begin
                /* Divide data Each 8Bits */
                toGatherBits[i][j].enq(in[(31-j*8):(24-j*8)]);
            end
        endrule
    end

    /* Gather Bits */
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule gatherBits;
            Vector#(16, Bit#(8)) in = replicate(0);
            Bit#(16) temp = 0;
            /* Get N th Bits */
            for (Bit#(8)j=0; j<16; j=j+1) begin
                toGatherBits[j][i].deq;
                in[j] = toGatherBits[j][i].first;
            end
            for (Bit#(8)j=0; j<8; j=j+1) begin
                temp = get_each_data(in,7-j);
                toMakeHeader[i*8+j].enq(temp);
            end
        endrule
    end

    /* Make Header */
    for (Bit#(6)i=0;i<32;i=i+1) begin
        rule makeHeader;
            Bit#(3) ti = 0;
            Bit#(3) header = 0;

            toMakeHeader[i].deq;
            let in = toMakeHeader[i].first;
            if (i < encodeBudget) begin
                ti = get_table_idx(in);
                header = msbCodeTable[ti];
                if (header == 4) begin
                    in[1] = 0;
                end

                merge_step1_header[i].enq(header);
                merge_step1_data[i].enq(in);
            end
        endrule
    end

    for (Bit#(6)i=0;i<8;i=i+1) begin
        rule merge_step1;
            Bit#(3) header = 0;
            Bit#(16) data = 0;
            Bit#(4) header_amount = 0;
            Bit#(7) data_amount = 0;

            Bit#(64) data_acu = 0;
            Bit#(12) header_acu = 0;
            for (Bit#(6)j=0;j<4 && i*4+j < encodeBudget ;j=j+1) begin
                Bit#(4) temp_h_amount;
                Bit#(7) temp_d_amount;

                merge_step1_data[i*4+j].deq;
                merge_step1_header[i*4+j].deq;
                data = merge_step1_data[i*4+j].first;
                header = merge_step1_header[i*4+j].first;

                /* get amount */
                temp_h_amount = get_h_amount(header);
                temp_d_amount = get_d_amount(header);
                /* accumulate */
                header_acu = header_acu << temp_h_amount;
                data_acu = data_acu << temp_d_amount;
                header_acu = header_acu | zeroExtend(header);
                data_acu = data_acu | zeroExtend(data);
                /* amount sum */
                header_amount = header_amount + temp_h_amount;
                data_amount = data_amount + temp_d_amount;
            end
            merge_step2_data[i].enq(data_acu);
            merge_step2_header[i].enq(header_acu);
            merge_step2_data_amount[i].enq(data_amount);
            merge_step2_header_amount[i].enq(header_amount);
        endrule
    end
    for (Bit#(6)i=0;i<4;i=i+1) begin
        rule merge_step2;
            Bit#(128) data_acu = 0;
            Bit#(24) header_acu = 0;
            Bit#(5) header_amount = 0;
            Bit#(8) data_amount = 0;
            if((i*2)*4 < encodeBudget) begin
                merge_step2_data[i*2].deq;
                merge_step2_header[i*2].deq;
                merge_step2_data_amount[i*2].deq;
                merge_step2_header_amount[i*2].deq;
                data_acu = zeroExtend(merge_step2_data[i*2].first);
                header_acu = zeroExtend(merge_step2_header[i*2].first);
                data_amount = zeroExtend(merge_step2_data_amount[i*2].first);
                header_amount = zeroExtend(merge_step2_header_amount[i*2].first);
            end
            if((i*2+1)*4 < encodeBudget) begin
                merge_step2_data[i*2+1].deq;
                merge_step2_header[i*2+1].deq;
                merge_step2_data_amount[i*2+1].deq;
                merge_step2_header_amount[i*2+1].deq;
                let d_amount = merge_step2_data_amount[i*2+1].first;
                let h_amount = merge_step2_header_amount[i*2+1].first;
                data_acu = (data_acu << d_amount) | zeroExtend(merge_step2_data[i*2+1].first);
                header_acu = (header_acu << h_amount) | zeroExtend(merge_step2_header[i*2+1].first);
                data_amount = data_amount + zeroExtend(d_amount);
                header_amount = header_amount + zeroExtend(h_amount);
            end
            toOut_d[i].enq(data_acu);
            toOut_d_amount[i].enq(data_amount);
            toOut_h[i].enq(header_acu);
            toOut_h_amount[i].enq(header_amount);
        endrule
    end
(* descending_urgency = "out, sendRemains" *)
    rule out;
        Bit#(328) output_buf = output_buffer;
        Bit#(328) temp = 0;

        Bit#(9) output_buf_off = output_buffer_off;
        Bit#(2) cycle = outCycle;

        if (cycle == 0) begin
            encodingExp.deq;
            let e = encodingExp.first;
            output_buf_off = output_buf_off + 12;
            output_buf = output_buf << 12;
            output_buf = output_buf | zeroExtend(e);
        end
        toOut_d[cycle].deq;
        toOut_h[cycle].deq;
        toOut_d_amount[cycle].deq;
        toOut_h_amount[cycle].deq;
        let d = toOut_d[cycle].first;
        let h = toOut_h[cycle].first;
        let d_m = toOut_d_amount[cycle].first;
        let h_m = toOut_h_amount[cycle].first;
        output_buf = output_buf << h_m;
        output_buf = output_buf | zeroExtend(h);
        output_buf = output_buf << d_m;
        output_buf = output_buf | zeroExtend(d);
        output_buf_off = output_buf_off + zeroExtend(d_m) + zeroExtend(h_m);

        if (output_buf_off > 164) begin
            outputQ.enq(truncateLSB(output_buf << (328-output_buf_off)));
            output_buf = output_buf << (328+(164-output_buf_off));
            output_buf = output_buf >> (328+(164-output_buf_off));
            output_buf_off = output_buf_off - 164;
        end
        
        output_buffer_off <= output_buf_off;
        output_buffer <= output_buf;

        if( (encodeBudget-1)/8 == zeroExtend(cycle)) begin
            outCycle <= 0;
        end else begin
            outCycle <= outCycle + 1;
        end
        outBuffCycle <= outBuffCycle + 1;
    endrule
    rule sendRemains(outBuffCycle / ((zeroExtend(encodeBudget) - 1) / 8 + 1) == totalMatrixCnt);
        lastOutputQ.enq(output_buffer);
        outBuffCycle <= 0;
    endrule

    /* Get input from Top.bsv */
    method Action put(Vector#(4, Bit#(64)) data);
        inputQ.enq(data);
    endmethod

    method Action put_encoding_size(Bit#(5) size);
        encodeBudget <= zeroExtend(size) + 1;
    endmethod

    method Action put_matrix_cnt(Bit#(32) cnt);
        totalMatrixCnt <= cnt;
    endmethod

    /* Send Output to Top.bsv */
    method ActionValue#(Bit#(164)) get;
        outputQ.deq;
        return outputQ.first;
    endmethod

    method ActionValue#(Bit#(328)) get_last;
        lastOutputQ.deq;
        return lastOutputQ.first;
    endmethod
endmodule

