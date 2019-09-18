import Connectable::*;
import FIFO::*;
import Vector::*;

interface ZfpIfc;
	method Action put(Vector#(4, Bit#(64)) data);
	method Action put_encoding_size(Bit#(6) size);
	method Action put_matrix_cnt(Bit#(32) cnt);
    method ActionValue#(Bit#(632)) get_last;
    method ActionValue#(Bit#(316)) get;
endinterface

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

function Bit#(19) dataCat(Bit#(3) h, Bit#(16) d, Bit#(19) outd);
    case(h)
        0: begin
            outd = outd << 1; 
            outd = outd | zeroExtend(d);
        end
        4: begin
            outd = outd << 1; 
            outd = outd | zeroExtend(d);
        end
        5: begin 
            outd = outd << 4; 
            outd = outd | zeroExtend(d);
        end
        6: begin 
            outd = outd << 8; 
            outd = outd | zeroExtend(d);
        end
        7: begin 
            outd = outd << 16; 
            outd = outd | zeroExtend(d);
        end
    endcase
    return outd;
endfunction
function Bit#(316) mergeCat (Bit#(316)outd, Bit#(164)d, Bit#(10) amount);
    outd = outd << amount;
    outd = outd | zeroExtend(d);
    return outd;
endfunction

function Bit#(164) mergeCat_8_data (Bit#(164)outd, Bit#(88)d, Bit#(10) amount);
    outd = outd << amount;
    outd = outd | zeroExtend(d);
    return outd;
endfunction

function Bit#(88) mergeCat_4_data (Bit#(88)outd, Bit#(19)d, Bit#(10) amount);
    outd = outd << amount;
    outd = outd | zeroExtend(d);
    return outd;
endfunction

function Bit#(64) intShift(Bit#(64) t);
    Bit#(1) s;
    s = t[63];
    t = t >> 1;
    t[63] = s;
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
function Bit#(64) int_to_uint(Bit#(64) t);
  return (t + 64'haaaaaaaaaaaaaaaa) ^ 64'haaaaaaaaaaaaaaaa;
endfunction

function Bit#(16) get_each_data (Vector#(16,Bit#(8)) in, Bit#(8) num);
    Bit#(16) acuBits;
    for (Bit#(8) i = 0; i < 16; i = i + 1) begin
        acuBits[i] = in[i][num];
    end
    return acuBits;
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

function Bit#(5) amountSum(Bit#(3) h, Bit#(5)amount);
    case(h)
        0: amount = amount + 1 + 1;
        4: amount = amount + 1 + 3;
        5: amount = amount + 4 + 3;
        6: amount = amount + 8 + 3;
        7: amount = amount + 16 + 3;
    endcase
    return amount;
endfunction

function Bit#(19) headCat(Bit#(3) h, Bit#(19) outd);
    if (h == 0) begin
        outd = outd << 1;
    end else begin
        outd = outd << 3;
        outd = outd | zeroExtend(h);
    end
    return outd;
endfunction

(* synthesize *)
module mkZfp (ZfpIfc);
    /* Rule to Rule FIFO */
	FIFO#(Vector#(4, Bit#(64))) inputQ <- mkFIFO;
	FIFO#(Bit#(316)) outputQ <- mkFIFO;
	FIFO#(Bit#(632)) lastOutputQ <- mkFIFO;
	FIFO#(Bit#(6)) sizeQ <- mkFIFO;

	FIFO#(Vector#(4, Bit#(64))) toGetFraction <- mkFIFO;
	FIFO#(Vector#(4, Bit#(64))) toMakeFixedPoint <- mkSizedFIFO(5);
	FIFO#(Vector#(4, Bit#(7))) shiftQ <- mkSizedFIFO(5);

	FIFO#(Vector#(4, Bit#(64))) toRowBlockTransform1Q <- mkFIFO;
	FIFO#(Vector#(4, Bit#(64))) toRowBlockTransform2Q <- mkFIFO;
	FIFO#(Vector#(4, Bit#(64))) toRowBlockTransform3Q <- mkFIFO;

	Vector#(16,FIFO#(Bit#(64))) toColBlockTransform1Q <- replicateM(mkFIFO);
	Vector#(16,FIFO#(Bit#(64))) toColBlockTransform2Q <- replicateM(mkFIFO);
	Vector#(16,FIFO#(Bit#(64))) toColBlockTransform3Q <- replicateM(mkFIFO);

	Vector#(16,FIFO#(Bit#(64))) toPermutation <- replicateM(mkFIFO);
	Vector#(16,FIFO#(Bit#(64))) toDivideBits <- replicateM(mkFIFO);
	Vector#(16,Vector#(8,FIFO#(Bit#(8)))) toGatherBits <- replicateM(replicateM(mkFIFO));
	Vector#(64,FIFO#(Bit#(16))) toMakeHeader <- replicateM(mkFIFO);

	Vector#(64,FIFO#(Bit#(16))) toMergeHeader_data<- replicateM(mkFIFO);
	Vector#(64,FIFO#(Bit#(3))) toMergeHeader_header_idx<- replicateM(mkFIFO);

	Vector#(64,FIFO#(Bit#(19))) toMerge_data<- replicateM(mkFIFO);
	Vector#(64,FIFO#(Bit#(5))) toMerge_amount <- replicateM(mkFIFO);

	Vector#(16,FIFO#(Bit#(88))) toMerge_8_data <- replicateM(mkFIFO);
	Vector#(16,FIFO#(Bit#(10))) toMerge_8_amount <- replicateM(mkFIFO);

	Vector#(8,FIFO#(Bit#(164))) toMerge_last_data <- replicateM(mkFIFO);
	Vector#(8,FIFO#(Bit#(10))) toMerge_last_amount <- replicateM(mkFIFO);

	Vector#(4,FIFO#(Bit#(316))) toOutBuff_data <- replicateM(mkFIFO);
	Vector#(4,FIFO#(Bit#(10))) toOutBuff_amount <- replicateM(mkFIFO);

    /* Exp FIFO */
	FIFO#(Vector#(4, Bit#(13))) exp <- mkSizedFIFO(5);
	FIFO#(Bit#(13)) maximumExp <- mkSizedFIFO(5);
	FIFO#(Bit#(13)) sendMaximumExp <- mkSizedFIFO(5);
	FIFO#(Bit#(13)) encodingExp <- mkSizedFIFO(31);

    /* Encoding Size, Cnt */
    Reg#(Bit#(8)) encodeBudget <- mkReg(0);
    Reg#(Bit#(32)) totalMatrixCnt <- mkReg(100);

    /* Encode Map */
    Vector#(5,Reg#(Bit#(3))) msbCodeTable <- replicateM(mkReg(0));
    msbCodeTable[0] <- mkReg(0); msbCodeTable[1] <- mkReg(4); msbCodeTable[2] <- mkReg(5); msbCodeTable[3] <- mkReg(6); msbCodeTable[4] <- mkReg(7);
    Vector#(5,Reg#(Bit#(5))) msbBitsTable <- replicateM(mkReg(3));
    msbBitsTable[0] <- mkReg(1);
    Vector#(5,Reg#(Bit#(5))) wbitsTable <- replicateM(mkReg(1));
    wbitsTable[2] <- mkReg(4); wbitsTable[3] <- mkReg(8); wbitsTable[4] <- mkReg(16);

	Reg#(Bool) weightDone <- mkReg(False);
	Reg#(Bit#(2)) cntMatrix <- mkReg(0);
	Reg#(Bit#(1)) setBudget <- mkReg(1);

	Reg#(Bit#(2)) cntTrans <- mkReg(0);

	Reg#(Bit#(32)) cntSequence <- mkReg(0);
	Reg#(Bit#(13)) expMax <- mkReg(0);
	Reg#(Bit#(13)) sendExpMax <- mkReg(0);
	Reg#(Bit#(13)) currentExpMax <- mkReg(0);

    /* Encoding Buffer and offset */
	Reg#(Bit#(632)) buffer <- mkReg(0);
	Reg#(Bit#(10)) offset <- mkReg(0);
	Reg#(Bit#(8)) bufferCycle <- mkReg(0);
    Reg#(Bit#(32)) outBuffCycle <- mkReg(0);

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
        if (cntMatrix + 1  == 0) begin
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

        cntMatrix <= cntMatrix + 1;

        exp.enq(matrixExp);
        toGetFraction.enq(in);
    endrule

    rule getFraction;
        toGetFraction.deq;
        Vector#(4, Bit#(64)) in = toGetFraction.first;
        Vector#(4, Bit#(64)) outd = replicate(0);
        Vector#(4, Bit#(52)) frac = replicate(0);

        /* Get Fraction from double data be using Bit operation <<, zeroextention, truncate
            * Make output vecotor and send to NextStep which is makeFixedPoint */
        for (Integer i = 0; i < 4; i = i+1) begin
            frac[i] = in[i][51:0];
            outd[i] = zeroExtend(frac[i]);
            outd[i] = zeroExtend(outd[i]<<11);
            /* Make Signed Extention */
            outd[i][63] = 1;
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
            if (term > 63) begin
                shift = 64;
            end else begin
                shift = truncate(term);
            end
            outd[i] = shift;
        end
        shiftQ.enq(outd);
    endrule

    rule makeFixedPoint;
        toMakeFixedPoint.deq; // Get 256Bits fraction data
        shiftQ.deq;
        let in = toMakeFixedPoint.first;
        let shift = shiftQ.first;

        Vector#(4, Bit#(64)) outd = replicate(0);
        /* Make Fixed Point by considering maximum Exp in Matrix */
        for (Integer i = 0; i < 4; i = i+1) begin
            if (shift[i] > 63) begin
                outd[i] = 0;
            end else begin
                outd[i] = in[i] >> shift[i];
            end
        end
        toRowBlockTransform1Q.enq(outd);
    endrule

    /* Row BlockTransform */
    rule rowBlockTransform1;
        toRowBlockTransform1Q.deq;
        Vector#(4, Bit#(64)) in = toRowBlockTransform1Q.first;
        Bit#(64) x = in[0];
        Bit#(64) y = in[1];
        Bit#(64) z = in[2];
        Bit#(64) w = in[3];
        x = (x+w); x = intShift(x); w = (w-x);
        z = (z+y); z = intShift(z); y = (y-z);
        x = (x+z);
        in[0] = x;
        in[1] = y;
        in[2] = z;
        in[3] = w;
        toRowBlockTransform2Q.enq(in);
    endrule

    rule rowBlockTransform2;
        toRowBlockTransform2Q.deq;
        Vector#(4, Bit#(64)) in = toRowBlockTransform2Q.first;
        Bit#(64) x = in[0];
        Bit#(64) y = in[1];
        Bit#(64) z = in[2];
        Bit#(64) w = in[3];
        x = intShift(x); z = (z-x);
        w = (w+y); w = intShift(w); y = (y-w);
        w = (w+ intShift(y)); 
        in[0] = x;
        in[1] = y;
        in[2] = z;
        in[3] = w;
        toRowBlockTransform3Q.enq(in);
    endrule

    rule rowBlockTransform3;
        toRowBlockTransform3Q.deq;
        Vector#(4, Bit#(64)) in = toRowBlockTransform3Q.first;
        Bit#(64) x = in[0];
        Bit#(64) y = in[1];
        Bit#(64) z = in[2];
        Bit#(64) w = in[3];
        y = (y - (intShift(w)));

        Bit#(5) idx = zeroExtend(cntTrans);
        toColBlockTransform1Q[idx*4].enq(x);
        toColBlockTransform1Q[idx*4+1].enq(y);
        toColBlockTransform1Q[idx*4+2].enq(z);
        toColBlockTransform1Q[idx*4+3].enq(w);
        cntTrans <= cntTrans + 1;

    endrule

    /* Colum Transform step 1*/
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule colTransform1;
            toColBlockTransform1Q[i+12].deq;
            toColBlockTransform1Q[i+8].deq;
            toColBlockTransform1Q[i+4].deq;
            toColBlockTransform1Q[i].deq;
            Bit#(64) x = toColBlockTransform1Q[i].first;
            Bit#(64) y = toColBlockTransform1Q[i+4].first;
            Bit#(64) z = toColBlockTransform1Q[i+8].first;
            Bit#(64) w = toColBlockTransform1Q[i+12].first;

            x = (x+w); x = intShift(x); w = (w-x);
            z = (z+y); z = intShift(z); y = (y-z);

           toColBlockTransform2Q[i].enq(x);
           toColBlockTransform2Q[i+4].enq(y);
           toColBlockTransform2Q[i+8].enq(z);
           toColBlockTransform2Q[i+12].enq(w);
        endrule
    end

    /* Colum Transform step2 */
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule colTransform3;
            toColBlockTransform2Q[i+12].deq;
            toColBlockTransform2Q[i+8].deq;
            toColBlockTransform2Q[i+4].deq;
            toColBlockTransform2Q[i].deq;
            Bit#(64) x = toColBlockTransform2Q[i].first;
            Bit#(64) y = toColBlockTransform2Q[i+4].first;
            Bit#(64) z = toColBlockTransform2Q[i+8].first;
            Bit#(64) w = toColBlockTransform2Q[i+12].first;

            x = (x+z); x = intShift(x); z = (z-x);
            w = (w+y); w = intShift(w); y = (y-w);

           toColBlockTransform3Q[i].enq(x);
           toColBlockTransform3Q[i+4].enq(y);
           toColBlockTransform3Q[i+8].enq(z);
           toColBlockTransform3Q[i+12].enq(w);
        endrule
    end

    /* Colum Transform step3 */
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule colTransform5;
            toColBlockTransform3Q[i+12].deq;
            toColBlockTransform3Q[i+8].deq;
            toColBlockTransform3Q[i+4].deq;
            toColBlockTransform3Q[i].deq;
            Bit#(64) x = toColBlockTransform3Q[i].first;
            Bit#(64) y = toColBlockTransform3Q[i+4].first;
            Bit#(64) z = toColBlockTransform3Q[i+8].first;
            Bit#(64) w = toColBlockTransform3Q[i+12].first;
            w = (w+ intShift(y)); y = (y - (intShift(w)));
            toPermutation[i].enq(x);
            toPermutation[i+4].enq(y);
            toPermutation[i+8].enq(z);
            toPermutation[i+12].enq(w);
        endrule
    end

    /* Permutation data */
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule permutation;
            toPermutation[i+12].deq;
            toPermutation[i+8].deq;
            toPermutation[i+4].deq;
            toPermutation[i].deq;
            Bit#(64) x = toPermutation[i].first;
            Bit#(64) y = toPermutation[i+4].first;
            Bit#(64) z = toPermutation[i+8].first;
            Bit#(64) w = toPermutation[i+12].first;

            x = int_to_uint(x);
            y = int_to_uint(y);
            z = int_to_uint(z);
            w = int_to_uint(w);

            toDivideBits[permutation_idx(i)].enq(x);
            toDivideBits[permutation_idx(i+4)].enq(y);
            toDivideBits[permutation_idx(i+8)].enq(z);
            toDivideBits[permutation_idx(i+12)].enq(w);
        endrule
    end

    /* Extract Each 8Bits(total 64) from 4x4 Matrix */
    for (Bit#(8)i=0;i<16;i=i+1) begin
        rule extractBits;
            /* Get ith element */
            toDivideBits[i].deq;
            let in = toDivideBits[i].first;
            for (Bit#(8)j=0;j<8;j=j+1) begin
                /* Divide data Each 8Bits */
	            toGatherBits[i][j].enq(in[(63-j*8):(56-j*8)]);
            end
        endrule
    end

    /* Gather Bits */
    for (Bit#(8)i=0;i<8;i=i+1) begin
        rule gatherBits;
            Vector#(16, Bit#(8)) in = replicate(0);
            Bit#(16) temp = 0;
            /* Get N th Bits */
            for (Bit#(8)j=0; j<16; j=j+1) begin
                toGatherBits[j][i].deq;
                in[j] = toGatherBits[j][i].first;
            end
            for (Bit#(8)j=0; j<8 && i*8+j < 64; j=j+1) begin
                temp = get_each_data(in,7-j);
                toMakeHeader[i*8+j].enq(temp);
            end
        endrule
    end

    /* Make Header */
    for (Bit#(8)i=0;i<64;i=i+1) begin
        rule makeHeader;
            Bit#(3) ti = 0;
            Bit#(3) header = 0;

            toMakeHeader[i].deq;
            let in = toMakeHeader[i].first;
            ti = get_table_idx(in);

            toMergeHeader_header_idx[i].enq(ti);
            toMergeHeader_data[i].enq(in);
        endrule
    end

    /* Merge Header and Data */
    for (Bit#(8)i=0;i<64;i=i+1) begin
        rule mergeHeader;
            Bit#(3) header = 0;
            Bit#(19) outd = 0;
            Bit#(5) amount = 0;

            toMergeHeader_data[i].deq;
            toMergeHeader_header_idx[i].deq;
            let in = toMergeHeader_data[i].first;
            let ti = toMergeHeader_header_idx[i].first;
            header = msbCodeTable[ti];

            outd = headCat(header,outd);
            outd = dataCat(header,in,outd);
            amount = amountSum(header,amount);

            toMerge_data[i].enq(outd);
            toMerge_amount[i].enq(amount);
        endrule
    end

    /* Merge data (gather each 4 compressed Data) */
    for (Bit#(8)i=0;i<16;i=i+1) begin
        rule mergeData;
            Bit#(88) outd = 0;
            Bit#(10) amount = 0;
                /* encode E bits */
            if (i == 0) begin
                encodingExp.deq;
                let e = encodingExp.first;
                outd = outd | zeroExtend(e);
                amount = amount + 12;
            end
            for (Bit#(8)j=0; j < 4 ; j=j+1) begin
                toMerge_amount[i*4+j].deq;
                toMerge_data[i*4+j].deq;
                let d = toMerge_data[i*4+j].first;
                let t_amount = toMerge_amount[i*4+j].first;

                if (i*4 + j < encodeBudget) begin
                    outd = mergeCat_4_data(outd,d,zeroExtend(t_amount));
                end else begin
                    t_amount = 0;
                end
                amount = amount + zeroExtend(t_amount);
            end

            toMerge_8_amount[i].enq(amount);
            toMerge_8_data[i].enq(outd);
        endrule
    end

    /* Merge data (gather each 8 compressed Data) */
    for (Bit#(8)i=0;i<8;i=i+1) begin
        rule mergeData;
            toMerge_8_amount[i*2].deq;
            toMerge_8_amount[i*2+1].deq;
            toMerge_8_data[i*2].deq;
            toMerge_8_data[i*2+1].deq;
            Bit#(164) outd = zeroExtend(toMerge_8_data[i*2].first);
            Bit#(10) amount = toMerge_8_amount[i*2].first;
            let d = toMerge_8_data[i*2+1].first;
            let t_amount = toMerge_8_amount[i*2+1].first;

            outd = mergeCat_8_data(outd,d,t_amount);
            amount = amount+t_amount;

            toMerge_last_amount[i].enq(amount);
            toMerge_last_data[i].enq(outd);
        endrule
    end
    /* Merge Last! data */
    for (Bit#(8)i=0;i<4;i=i+1) begin
        rule mergeLastData;
            toMerge_last_amount[i*2].deq;
            toMerge_last_amount[i*2+1].deq;
            toMerge_last_data[i*2].deq;
            toMerge_last_data[i*2+1].deq;
            if (i*16 < encodeBudget) begin
                Bit#(316) outd = zeroExtend(toMerge_last_data[i*2].first);
                Bit#(10) amount = toMerge_last_amount[i*2].first;
                let d = toMerge_last_data[i*2+1].first;
                let t_amount = toMerge_last_amount[i*2+1].first;

                outd = mergeCat(outd,d,t_amount);
                amount = amount+t_amount;

                toOutBuff_amount[i].enq(amount);
                toOutBuff_data[i].enq(outd);
            end
        endrule
    end
(* descending_urgency = "outBuff, sendRemains" *)
    rule outBuff;
        Bit#(632) cbuf = buffer;
        Bit#(10) coff = offset;
        Bit#(8) cycle = bufferCycle;
        Bit#(632) temp = 0;

        toOutBuff_amount[cycle].deq;
        toOutBuff_data[cycle].deq;
        let amount = toOutBuff_amount[cycle].first;
        let d = toOutBuff_data[cycle].first;
        temp = zeroExtend(d);
        temp = temp << (632-amount);
        temp = temp >> coff;
        cbuf = cbuf | temp;
        coff = coff + amount;
        if (coff > 316) begin
            outputQ.enq(cbuf[631:316]);
            offset <= coff - 316;
            cbuf = zeroExtend(cbuf[315:0]);
            buffer <= cbuf << 316;
        end else begin
            offset <= coff;
            buffer <= cbuf;
        end
        if( (encodeBudget-1)/16 == cycle) begin
            bufferCycle <= 0;
        end else begin
            bufferCycle <= bufferCycle + 1;
        end
        outBuffCycle <= outBuffCycle + 1;
    endrule

    rule sendRemains(outBuffCycle / ((zeroExtend(encodeBudget) - 1) / 16 + 1) == totalMatrixCnt);
        lastOutputQ.enq(buffer);
        outBuffCycle <= 0;
    endrule

    /* Get input from Top.bsv */
    method Action put(Vector#(4, Bit#(64)) data);
        inputQ.enq(data);
    endmethod

    method Action put_encoding_size(Bit#(6) size);
        encodeBudget <= zeroExtend(size) + 1;
    endmethod

    method Action put_matrix_cnt(Bit#(32) cnt);
        totalMatrixCnt <= cnt;
    endmethod

    /* Send Output to Top.bsv */
    method ActionValue#(Bit#(316)) get;
        outputQ.deq;
        return outputQ.first;
    endmethod

    method ActionValue#(Bit#(632)) get_last;
        lastOutputQ.deq;
        return lastOutputQ.first;
    endmethod
endmodule
