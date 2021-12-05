Memory layout
r r r r 0 0 0 0 i1 i1 i1 i1 0 l1 0 0 0 0 0 0 0 0 0 i2 i2 i2 i2 0 l2

>>>>>>> Move to the first nonresult address

>+[-<,+[[->+<] Check if the character read is EOF (Must stand before i1 with i1s zeroed)
>-----------[>,----------] Read characters until we read \n
<[++++++++++<] Move back before the first read char and increment everything back to the original value

>>>> Move to the 4th char of the input

>>[-]++++<< Set l1 to 4

Branch: if the last char is zero set length 3
+ Increment last char
[>>>+>+<<<<-]>>>>[<<<<+>>>>-] Copy the last char to the cell after l1: stand at l1p2
>+<<  Set l1p3 to 1 and move back to copied value
[
	-[>]>> Decrement the copied value by 1 and move to either l1p3/4
	[<<<[-]+++>>>[-]>] If we are on l1p3 then set l1 to 3: set l1p3 to 0
] Always end up on l1p4
<<<<<<- Decrement the last char

Compare the input to the previously read number:
Compare length and then digits

<[<] Move to the front of i1s
>[.>] Print the read characters

<[<]>>>>>>.[-]< Print l1 and zero it out

[-]++++++++++.[-] Print \n at the end

<[[-]<] Move to the front and set everything to zero


>+<] Keep on reading until we find EOF (this requires that all bytes be set to 0 before reading the next line)
>]