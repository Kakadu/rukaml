##### Debugging

    dune b test/fib_loop.exe && \
        qemu-riscv64 -L /usr/riscv64-linux-gnu -cpurv64 -g 1234 test/fib_loop.exe
    (cd test && gdb-multiarch fib_loop.exe)