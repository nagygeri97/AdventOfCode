>+[-<,+[[->+<] Check if the character read is EOF
>-----------[>,----------] Read characters until we read \n
<[++++++++++<] Move back before the first read char and increment everything back to the original value

>[.>] Print the read characters
[-]++++++++++.[-] Print \n at the end
<[[-]<] Move to the front and set everything to zero

>+<] Keep on reading until we find EOF (this requires that all bytes be set to 0 before reading the next line)
>]