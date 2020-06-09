// elxrVM.cpp : This file contains the 'main' function. Program execution begins and ends there.
// Attempt at writing an RISC code virtualizer

//Will start out as LC3 since I'm going to be using a tutorial for this: https://justinmeiners.github.io/lc3-vm/

#include <stdio.h>
#include <stdint.h>
#include <signal.h>
#include <Windows.h>
#include <conio.h>

HANDLE hstdin = INVALID_HANDLE_VALUE;



//LC3 has 10 total registers, each 16 bits wide.

/* Designated Roles
R0-R7: General Purpose
PC: Program Counter
COND: Condition Flag
*/
enum
{
	r0 = 0,
	r1,
	r2,
	r3,
	r4,
	r5,
	r6,
	r7,
	r_pc,
	r_cond,
	r_count
};


//Instruction Set
//--LC3 has 16 opcodes. Each opcode represents one task. Each instruction
//  is 16 bits long, with the left 4 bits storing the actual opcode.
//  The rest of the bits are used to store parameters.
enum
{
	op_br = 0, //branch
	op_add,    //add
	op_ld,     //load
	op_st,     //store
	op_jsr,    //jump register
	op_and,    //bitwise and
	op_ldr,    //load direct register
	op_str,    //store register
	op_rti,    //unused
	op_not,    //bitwise not
	op_ldi,    //load indirect
	op_sti,    //store indirect
	op_jmp,    //jump
	op_res,    //reserved
	op_lea,    //lead effective address
	op_trap    //execute trap
};


//Condition Flags
//--The r_cond register stores the condition flag from the most
//  recently executed calculation. This allows checks such as if(x>0)
//
//--Each CPU has a variety of these flags to signal various situations.
//  LC3 only uses 3, which indicate the sign of the previous calculation
enum
{
	fl_pos = 1 << 0,
	fl_zro = 1 << 1,
	fl_neg = 1 << 2
};


//Trap Codes
//--LC3 provides a few predefined routines for performing common I/O
//  with devices. For example, there are routines to grab keyboard
//  input. These trap routines act as the "OS" or API of LC3. Each
//  routine has a code associated with it (similar to an opcode)
enum
{
	t_getc = 0x20,  // gets character from keyboard, non echo
	t_out = 0x21,   // outputs a character
	t_puts = 0x22,  // outputs a char string
	t_in = 0x23,    // gets a character from keyboard, echo
	t_putsp = 0x24, // output a byte string
	t_halt = 0x25   // halt the program
};


//Memory mapped registers
//--Some registers are not available from the normal register table,
//  Instead, an address space is reserved for them in memory. In order
//  to access these registers, one must read and write to their location
//--LC3 has two that need to be implemented, which are for keyboard
//  status (KBSR) and keyboard data (KBDR)
//----Although one can request input from getc(), this blocks 
//    execution until input is received. These registers poll the
//    state of the device and continue execution, leaving the program
//    responsive
enum
{
	kbsr = 0xFE00,
	kbdr = 0xFE02
};


uint16_t memory[UINT16_MAX];
uint16_t reg[r_count];
int running = 1;


uint16_t sign_ex(uint16_t x, int bc)
{
	if ((x >> (bc - 1)) & 1)
		x |= (0xFFFF << bc);
	return x;
}

uint16_t swap(uint16_t x)
{
	return (x << 8) | (x >> 8);

}

void update_flags(uint16_t r)
{
	if (reg[r] == 0)
	{
		reg[r_cond] = fl_zro;
	}
	else if (reg[r] >> 15)
	{
		reg[r_cond] = fl_neg;
	}
	else
	{
		reg[r_cond] = fl_pos;
	}
}

namespace t
{
	void put()
	{
		uint16_t* c = memory + reg[r0];
		while (*c)
		{
			putc((char)* c, stdout);
			++c;
		}
		fflush(stdout);
	}
	void getc()
	{
		reg[r0] = (uint16_t)getchar();
	}

	void out()
	{
		putc((char)reg[r0], stdout);
		fflush(stdout);
	}

	void in()
	{
		printf("Enter a character: ");
		char c = getchar();
		putc(c, stdout);
		reg[r0] = (uint16_t)c;
	}

	void putsp()
	{
		uint16_t* c = memory + reg[r0];
		while (*c)
		{
			char c1 = (*c) & 0xFF;
			putc(c1, stdout);
			char c2 = (*c) >> 0x8;
			if (c2) putc(c2, stdout);
			++c;
		}
		fflush(stdout);
	}

	void halt()
	{
		puts("Halt called");
		fflush(stdout);
		running = 0;
	}
};

void read_image_file(FILE* file)
{
	uint16_t origin;
	fread(&origin, sizeof(origin), 1, file);
	origin = swap(origin);

	uint16_t max_read = UINT16_MAX - origin;
	uint16_t* p = memory + origin;
	size_t read = fread(p, sizeof(uint16_t), max_read, file);

	while (read-- > 0)
	{
		*p = swap(*p);
		++p;
	}
}

uint16_t check_key()
{
	return WaitForSingleObject(hstdin, 1000) == WAIT_OBJECT_0 && _kbhit();
}

int read_image(const char* image_path)
{
	FILE* file = fopen(image_path, "rb");
	if (!file) return 0;
	read_image_file(file);
	fclose(file);
	return 1;
}

void mem_write(uint16_t address, uint16_t val)
{
	memory[address] = val;
}

uint16_t mem_read(uint16_t address)
{
	if (address == kbsr)
	{
		if (check_key())
		{
			memory[kbsr] = (1 << 15);
			memory[kbdr] = getchar();
		}
		else
		{
			memory[kbsr] = 0;
		}
	}
	return memory[address];
}

DWORD mode, old_mode;

void disable_input_buffering()
{
	hstdin = GetStdHandle(STD_INPUT_HANDLE);

	GetConsoleMode(hstdin, &old_mode);
	mode = old_mode ^ ENABLE_ECHO_INPUT ^ ENABLE_LINE_INPUT;

	SetConsoleMode(hstdin, mode);
	FlushConsoleInputBuffer(hstdin);
}

void restore_input_buffering()
{
	SetConsoleMode(hstdin, old_mode);
}

void handle_interrupt(int signal)
{
	restore_input_buffering();
	printf("\n");
	exit(-2);
}

//Add instruction
/*
  Two different encoding mechanisms for op_add


  4      3     3     1   2    3
  opcd | DR  | SR1 | 0 | 00 | SR2 
  The first 4 bits as always are the opcode: in this case 0001
  The next 3 bits are the destination register
  SR1 contains the register of the first number to add
  The bit after SR1 represents the use of immediate mode
  The last 3 bits are the register of the second number to add

  Example: ADD R2, R0, R1 ; add R1 to R0 and save it into R2

  4      3    3     1   5
  opcd | DR | SR1 | 1 | imm5
  The first 4 bits as always are the opcode: in this case 0001
  DR is the same as before
  SR1 is the same as before
  The bit after SR1 is now set to 1, which denotes use of immediate mode
  The last 5 bits denote the value to be added to SR1

  Example: ADD R0, R0, 1 ; add 1 to R0 and store into R0
*/

void add(uint16_t i)
{
	//Get the DR
	uint16_t r0 = (i >> 9) & 0x7;
	/*
	  Shifting the instruction right 9 bits leaves you with
	     4      3
	     opcd | DR 
	Ex:  0001 | 001

	  By bitwise AND'ing it with 0x7 (0000111), you'll leave
	  only the values in DR present

	      0001001
	  AND 0000111
	  -----------
	      0000001
	*/

	//Get the SR1
	uint16_t r1 = (i >> 6) & 0x7;
	/*
	  The same logic applies for this, the right shift is just
	  changed to grab the SR1 instead of the DR
	*/

	//Get the IMM flag
	uint16_t imm = (i >> 5) & 0x1;
	/*
	  The same logic applies for this, the right shift is just
	  changed to grab the IMM instead of the DR, and the bitwise
	  AND is changed to only check for 1 bit
	*/

	if (imm)
	{
		uint16_t imm5 = sign_ex(i & 0x1F, 5);
		reg[r0] = reg[r1] + imm5;
	}
	else
	{
		uint16_t r2 = i & 0x7;
		reg[r0] = reg[r1] + reg[r2];
	}

	update_flags(r0);
}

//Load indirect instruction
/*
  One encoding type for op_ldi

  4      3     9
  opcd | DR  | offset    
  The first 4 bits as always are the opcode: in this case 1010
  DR is the same as before
  The last 8 bits are an immediate value, which represents the 
  address to load from

  If we create a quick example of how this would work

  char* far_data = "apple";

  Address | Label    | value
  0x120   | far_data | 0x420
  0x420   | string   | 'a'

  PC = 0x100
  LDI R0, 0x020 ; would load 'a' into R0
*/

void ldi(uint16_t i)
{
	//Get DR
	uint16_t r0 = (i >> 9) & 0x7;

	//Get offset
	uint16_t off = sign_ex(i & 0x1F, 9);
	/*
	  We need to sign extend the offset as its address is
	  currently 9 bits long, and LC3 uses 16 bit memory space
	*/

	//Write the value the object into r0
	reg[r0] = mem_read(mem_read(reg[r_pc] + off));
	/*
	  We read memory twice as we need to dereference both pointers,
	  the instruction parameter as well as its value, Hence why this 
	  instruction is an indirect load call. 

	*/

	update_flags(r0);
}

//AND instruction
/*
  Two different encoding mechanisms for op_and

  4      3     3     1   2    3
  opcd | DR  | SR1 | 0 | 00 | SR2 
  The first 4 bits as always are the opcode: in this case 0101
  DR is the same as before
  SR1 is the same as before
  The bit after SR1 is the same as before
  SR2 is the same as before

  Example: AND R2, R3, R4 ; store R3 & R4 in R0

  4      3     3     1   5
  opcd | DR  | SR1 | 1 | imm5   
  The first 4 bits as always are the opcode: in this case 0101
  DR is the same as before
  SR1 is the same as before
  The bit after SR1 is now set for immediate mode
  The last 5 bits denote the value to be added to SR1

  Example: AND R2, R3, 0x7 ; store R3 & 0x7 in R2 
*/

void AND(uint16_t i)
{
	//Get DR
	uint16_t r0 = (i >> 9) & 0x7;

	//Get SR1
	uint16_t r1 = (i >> 6) & 0x7;

	//Get IMM flag
	uint16_t imm = (i >> 5) & 0x1;

	if (imm)
	{
		uint16_t imm5 = sign_ex(i & 0x1F, 5);
		reg[r0] = r1 & imm5;
	}
	else
	{
		uint16_t r2 = i & 0x7;
		reg[r0] = r1 & r2;
	}

	update_flags(r0);
}

//NOT instruction
/*

  One encoding type for this instruction
  
  4      3     3     1   5
  opcd | DR  | SR1 | 1 | 11111 |
  The first 4 bits as always are the opcode: in this case 1001
  DR is the same as before
  SR1 is the same as before

  Example: NOT R4, R2 ; ~R2 is stored in R4
*/

void NOT(uint16_t i)
{
	uint16_t r0 = (i >> 9) & 0x7;
	uint16_t r1 = (i >> 6) & 0x7;

	reg[r0] = ~r1;
	/*
	  The ~ operator, bitwise NOT, works similarly to the AND
	  operation, but does the exact opposite. All bits are
	  logically flipped.

	  NOT 0111 -> 1000

	*/

	update_flags(r0);
}

//Branch Instruction
/*
  One encoding type for op_br

  4      1   1   1    9
  opcd | n | z | p | offset
  The first 4 bits as always are the opcode: in this case 0000
  Bits n-p are states used to check whether they should be tested
  The last 9 bits are the offset added to PC as the branch location

  Example: BR(xyz) LOOP ; branch to LOOP if the last result was xyz
           BR NEXT ; branch to next unconditionally
*/

void branch(uint16_t i)
{
	//Get offset
	uint16_t off = sign_ex(i & 0x1FF, 9);

	//Get condition flag
	uint16_t cond = (i >> 9) & 0x7;

	if (cond & reg[r_cond])
		reg[r_pc] += off;
}

//Jump instruction
/*

  Two types of encoding are used for op_jmp (jump and return)

  JMP
  4      3     3     6
  opcd | 000 | BR  | 000000
  The first 4 bits as always are the opcode: in this case 1100
  The 3 BR bits are the base register

  Example: JMP R2 ; PC is set to R2

  RET
  4      3     3     6
  opcd | 000 | 111 | 000000
  The first 4 bits as always are the opcode: in this case 1100
  The 3 BR bits are now an indicator of the RET statement

  Example: RET ; PC is set to R7*

  *The RET instruction is a special jump instruction, R7's content
  is the linkage to the instruction following the subroutine call
*/

void jmp(uint16_t i)
{
	//Get jmp location
	uint16_t r1 = (i >> 6) & 0x7;

	reg[r_pc] = reg[r1];
}

//Jump Register instruction
/*
  Two types of encoding are used for op_jsr

  JSR
  4      1   11
  opcd | 1 | offset
  The first 4 bits as always are the opcode: in this case 0100
  The next bit determines whether a base register or the direct
  address will be used to load the next subroutine address into R7

  Example: JSR QUEUE ; Put the address of QUEUE into R7, JMP QUEUE

  JSRR
  4      1   2    3     6
  opcd | 0 | 00 | BR  | 000000
  The first 4 bits as always are the opcode: in this case 0100
  The next bit is the same as before
  BR is the same as before

  Example: JSRR R3 ; Puts the address of R3 into R7, JMP R3
*/

void jsr(uint16_t i)
{
	//Get DR
	uint16_t r1 = (i >> 6) & 0x7;

	//Get offset
	uint16_t off = sign_ex(i & 0x7FF, 11);

	//Get RET flag
	uint16_t flag = (i >> 11) & 1;
	
	//Save address before branch into R7
	reg[r7] = reg[r_pc];

	if (flag)
	{
		reg[r_pc] += off; //JSR
	}
	else
	{
		reg[r_pc] = reg[r1]; //JSRR
	}
}

//Load instruction
/*
  One encoding type for op_ld

  4      3     9
  opcd | DR  | offset
  The first 4 bits as always are the opcode: in this case 0010
  DR is the same as before
  The last 9 bits are the same as before.

  Example: LD R4, VAL ; write the value stored at VAL into R4
*/
void ld(uint16_t i)
{
	//Get DR
	uint16_t r0 = (i >> 0x9) & 0x7;

	//Get offset
	uint16_t off = sign_ex(i & 0x1FF, 9);

	reg[r0] = mem_read(reg[r_pc] + off);

	update_flags(r0);
}

//Load Register instruction
/*
  One encoding type of op_ldr

  4      3     3     6
  opcd | DR  | BR  | offset
  The first 4 bits as always are the opcode: in this case 0110
  DR is the same as before
  BR is the same as before
  The last 6 bits are the offset from the base register

  Example: LDR R4, R2, 0x5 ; load the value at R2 - 5 in R4
*/

void ldr(uint16_t i)
{
	//Get DR
	uint16_t r0 = (i >> 9) & 0x7;

	//Get BR
	uint16_t r1 = (i >> 6) & 0x7;

	//Get offset
	uint16_t off = sign_ex(i & 0x3F, 6);

	reg[r0] = mem_read(reg[r1] + off);

	update_flags(r0);
}

//Load Effective Address instruction
/*
  One encoding type for op_lea

  4      3     9
  opcd | DR  | offset
  The first 4 bits as always are the opcode: in this case 1110
  DR is the same as before
  The last 9 bits are the offset from PC to the address to be 
  loaded into DR

  Example: LEA R4, TARGET ; TARGET's pointer is loaded into R4
*/

void lea(uint16_t i)
{
	uint16_t r0 = (i >> 9) & 0x7;

	uint16_t off = sign_ex(i & 0x1FF, 9);

	reg[r0] = reg[r_pc] + off;

	update_flags(r0);
}

//Store instruction
/*
  One encoding type for op_st

  4      3     9
  opcd | SR  | offset
  The first 4 bits as always are the opcode: in this case 0011
  The SR is the same as before
  The next 9 bits are the offset from PC where SR's contents
  should be stored

  Example: ST R4, HERE ; load R4 into the value at PC + offset
*/

void st(uint16_t i)
{
	//Get SR
	uint16_t r0 = (i >> 9) & 0x7;

	//Get offset
	uint16_t off = sign_ex(i & 0x1FF, 9);

	mem_write(reg[r_pc] + off, reg[r0]);
}

//Store Indirect instruction
/*
  One encoding type for op_sti

  4      3     9
  opcd | SR  | offset
  The first 4 bits as always are the opcode: in this case 0011
  The SR is the same as before
  The next 9 bits are the offset from PC where SR's contents
  should be stored

  Example: STI R4, NOT_HERE ; Load R4 into the value of NOT_HERE's value
                              i.e NOT_HERE holds 0x10 -> PC is at 0x300
							  -> value at 0x310 is 0x500 -> write R4 to
							  0x500
*/

void sti(uint16_t i)
{
	uint16_t r0 = (i >> 9) & 0x7;

	uint16_t off = sign_ex(i & 0x1FF, 9);

	mem_write(mem_read(reg[r_pc] + off), reg[r0]);
}

//Store Register instruction
/*
  4      3     3     6
  opcd | SR  | BR  | offset
  The first 4 bits as always are the opcode: in this case 0111
  SR is the same as before
  BR is the same as before
  The last 6 bits are the offset from the base register

  Example: STR R4, R2, 0x5 ; store R4 in the address from the 
                             value at R2 + 5
*/

void str(uint16_t i) 
{
	uint16_t r0 = (i >> 9) & 0x7;

	uint16_t r1 = (i >> 6) & 0x7;

	uint16_t off = sign_ex(i & 0x3F, 6);

	mem_write(reg[r1] + off, reg[r0]);
}

//Trap instruction
/*
  Only one encoding type for op_trap

  4      4      8
  opcd | 0000 | trapvect
  The first 4 bits as always are the opcode: in this case 1111
  The last 8 bytes are the trap instruction

  Example: TRAP 0x23 ; Executes t_in, whose starting address
                       will be at 0x0023*

  *Memory locations from 0x0 to 0x00FF are available to contain 
   syscalls specified by the trap vector. This region is called
   the Trap Vector Table.
*/

void trap(uint16_t i)
{
	//Get the trap vector
	uint16_t code = i & 0xFF;

	switch (code)
	{
	case t_getc:
		t::getc();
		break;
	case t_out:
		t::out();
		break;
	case t_puts:
		t::put();
		break;
	case t_in:
		t::in();
		break;
	case t_putsp:
		t::putsp();
		break;
	case t_halt:
		t::halt();
		break;
	}
}

int main(int argc, const char* argv[])
{
	//Load the arguments
	if (argc < 2)
	{
		printf("Usage: elixirvm [image-file.obj]");
		exit(2);
	}

	for (int j = 1; j < argc; ++j)
	{
		if (!read_image(argv[j]))
		{
			printf("failed to load image: %s\n", argv[j]);
			exit(1);
		}
	}

	//Setup
	signal(SIGINT, handle_interrupt);
	disable_input_buffering();

	/* set the PC register to the starting position */
	/* 0x3000 is default*/
	auto pc_start = 0x3000;
	reg[r_pc] = pc_start;

	
	while (running)
	{
		//Fetch
		UINT16 i = mem_read(reg[r_pc]++);
		UINT16 op = i >> 12;

		switch (op)
		{
		case op_add:
			add(i);
			break;
		case op_and:
			AND(i);
			break;
		case op_not:
			NOT(i);
			break;
		case op_br:
			branch(i);
			break;
		case op_jmp:
			jmp(i);
			break;
		case op_jsr:
			jsr(i);
			break;
		case op_ld:
			ld(i);
			break;
		case op_ldi:
			ldi(i);
			break;
		case op_ldr:
			ldr(i);
			break;
		case op_lea:
			lea(i);
			break;
		case op_st:
			st(i);
			break;
		case op_str:
			str(i);
			break;
		case op_trap:
			trap(i);
			break;
		case op_res:
			break;
		case op_rti:
			break;
		default:
			break;
		}
	}
	//Shutdown
	restore_input_buffering();
}