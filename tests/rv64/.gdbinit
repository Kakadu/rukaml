
#tui new-layout example {-horizontal regs 1 asm 1} 2 status 0 cmd 1
#tui layout example
#set history save
#set pagination off
set architecture riscv:rv64
set sysroot /usr/riscv64-linux-gnu

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
b test
#b _start
c
#memory watch $sp
#display/8gx $sp
#memory watch $sp 10 qword
#gef config context.layout "regs   memory"
