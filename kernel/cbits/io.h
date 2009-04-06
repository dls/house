typedef unsigned char Byte;
typedef unsigned short Word;
typedef unsigned int Long;
typedef Word Port;

Byte inb(Port port);
Word inw(Port port);
Long inl(Port port);

void outb (Port port, Byte value);
void outw (Port port, Word value);
void outl (Port port, Long value);

void enableInterrupts();
void disableInterrupts();

