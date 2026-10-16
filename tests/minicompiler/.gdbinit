
set history save
set pagination off
set architecture riscv:rv32
set sysroot /mnt/work2/asp/sc-dt/riscv-gcc/sysroot/usr

pi gdb.execute('source %s' % '/mnt/oldoldhome/kakadu/prog/gef/gef.py')
#gef config context.layout "-legend regs stack code args -source -threads -trace extra memory"
gef config context.layout "legend stack regs code -args source -threads -trace extra "
# Larger stack view
gef config context.nb_lines_stack 10
#target remote :1234
gef-remote localhost 1234
#set style address foreground white
theme registers_register_name blue bold
#x/8xg $sp
#layout reg
#layout asm
#memory watch 0x555555560ad0 0x8 qword
#memory watch 0x555555560b90 0x8 qword
#tui enable

#focus cmd
b main
#b _start
c
#memory watch $sp
#display/8gx $sp
#memory watch $sp 10 qword
#gef config context.layout "regs   memory"
