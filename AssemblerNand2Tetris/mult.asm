// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.
//@R1
//D=M
//@R0
//D=D+M
//@R2
//M=D
//(END)
//@END
//0;JMP
@R2       // clear r2
M=0
(LOOP)    
@R0      // set d = to the contents of r0 which is
D=M      //  the counter (one of the multipliers)
@END      // if d = 0 got to end loop because
D;JEQ     //  once counter is zero we are done
@R1      // set d = to contents of r1 which is
D=M      //  the other multiplier
@R2      // add d (r1 value) to r2 (again)
M=M+D
@R0       // subtract 1 from r0
M=M-1
@R0       // set d = new counter value
D=M
@LOOP    // if counter > 0 go back to top of loop
D;JGT
(END)     // endless loop to end
@END
0;JMP