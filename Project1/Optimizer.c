/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

int instructionSize(Instruction * head){
	Instruction * ptr = NULL;
	int counter = 0;
	for(ptr=head; ptr!=NULL; ptr=ptr->next){
		counter++;
	}
	return counter;
}

int isCritical(int arr[], int size, int field1){
	int i;
	for(i=0;i<size;i++){
		if(arr[i] == field1){
			return 1;
		} 
	}
	return 0;
}

int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	/* YOUR CODE GOES HERE */
	//use head and traverse through file to get all of the instructions.  Eliminate any "dead" instructions
	//STDIN is like a file pointer to tinyL.out

	int size = instructionSize(head);
	int criticalVal[size];
	int n;
	for(n=0;n<size;n++){ //zeroes out the array
		criticalVal[n] = 0;
	}
	int index=0;
	//int criticalVal2; //will be used only when we encounter ADD,SUB, or MUL instructions
	Instruction * instr;
	//Instruction * instr2; //temp pointer
	instr = LastInstruction(head);
	if(instr->opcode != WRITE){ //needs a write operation
		printf("No WRITE operation found\n");
		exit(EXIT_FAILURE);
	}
	instr->critical = '1';
	criticalVal[index] = instr->field1;
	index++;
	instr = instr->prev; //now we need to find the store that corresponds to the criticalVal

	while(instr != NULL){ //purpose of this while loop is to find the STORE that goes with our final WRITE operation;  IGNORES any OTHER instructions between this STORE and WRITE
		if( (instr->opcode == STORE) && (isCritical(criticalVal, size, instr->field1) == 1) ){ //we found the STORE that connects to our write
			instr->critical = '1';
			criticalVal[index] = instr->field2;
			index++;
			instr = instr->prev;	
			break;
		}else{
			instr = instr->prev;
		}	
	}


	/*THIS IS WORKING PROPERLY*/
	//now we just need to find all of the critical instructions.	
	// *instr could now we pointing to any arbitrary instruciton now
	while(instr != NULL){ //set up like this because we can have multiple reads
		if(instr->opcode == READ){ //can have MULTIPLE READS, but even READS that are extraneous are still considered a critical instruction.
			instr->critical = '1';			
		}else if(isCritical(criticalVal, size, instr->field1) == 1){
			instr->critical = '1';
			if(instr->opcode == LOADI){ // DON'T put these field2's into the criticalVal Array
				instr = instr->prev;
				continue;				
			}else if(instr->opcode == ADD || instr->opcode == MUL || instr->opcode == SUB){ //then field2 and field3 have values to input as a critical value
				if(isCritical(criticalVal, size, instr->field2) == 0){ // checks if field2 is already in the criticalVal array, if "== 0" that means element is NOT in array yet
					criticalVal[index] = instr->field2;
					index++;
				}
				if(isCritical(criticalVal, size, instr->field3) == 0){ // checks if field3 is already in the criticalVal array
					criticalVal[index] = instr->field3;
					index++;
				}
			}else if(instr->opcode == STORE || instr->opcode == LOAD){
				if(isCritical(criticalVal, size, instr->field2) == 0){ // checks if field2 is already in the criticalVal array
					criticalVal[index] = instr->field2;
					index++;
				}
			}
		}else{ //this means that the field1 was not a critical element, thus we ignore this instruction
			instr = instr->prev;
		}
		instr = instr->prev;
	}
	


	/*THIS WORKS FINE*/
	//by now we should have identified all critical instrctions.  All of these critical instructions should have a (char critical = '1')
	//think of head like a pointer to a linked list.
	instr = head;
	Instruction * prev;
	Instruction * next;
	while(instr != NULL){ //
		if(instr->critical != '1'){ //then this is not a critical value
			if(instr == head){ //then the first node of the linked list is not a critical value
				head = instr->next;
				instr = head;
				instr = instr->next;
				continue;
			}

			next = instr->next;
			prev = instr->prev;
			prev->next = instr->next; //relinking the pointers like this essentially deletes the node in the linkedlist (or non-critical instruction).
			next->prev = instr->prev; //relinking prevs too

			//now we have to free the node w/o losing the instr pointer -->prevents memory leaks
			free(instr);
			instr = next;
			continue;
			
		}
		instr = instr->next;
	}

	
	
	// How about, every time we see a critical value, we will recurisvely traverse through tree and find where field1 contains that critical value.  If field1 is a LOAD or LOADI instruction,
	// then we just end the recursion there and return, otherwise, we make field2 and field3 critical values and call recursive calls on those critical values.
	
	
	//prints out whats remaining of the list using the *head
	if (head) {
		PrintInstructionList(stdout, head);
		DestroyInstructionList(head);
	}
	return EXIT_SUCCESS;
}

