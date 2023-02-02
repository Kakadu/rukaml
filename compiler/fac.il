//.module fac.exe
  .class public auto ansi abstract sealed Fac
  {
    .method public static default int32 fac (int32 n)  cil managed
    {
        .maxstack 8
        IL_0000:  ldarg.0
        IL_0001:  ldc.i4.1
        IL_0002:  bne.un.s IL_0006

        IL_0004:  ldarg.0
        IL_0005:  ret
        IL_0006:  ldarg.0
        IL_0007:  ldarg.0
        IL_0008:  ldc.i4.1
        IL_0009:  sub
        IL_000a:  call int32 class Fac::fac(int32)
        IL_000f:  mul
        IL_0010:  ret
    } // end of method Fac::fac

    .method public static
           default int32 main (string[] _arg1)  cil managed
    {
        // Method begins at RVA 0x2064
        .entrypoint
        // Code size 13 (0xd)
        .maxstack 8
        ldc.i4.5
        call int32 class Fac::fac(int32)
        call void class [mscorlib]System.Console::WriteLine(int32)
        ldc.i4.0
        ret
    }
  }

.namespace '<StartupCode$fac>'
{
  .class private auto ansi abstract sealed $Fac
  	extends [mscorlib]System.Object
  {

  } // end of class <StartupCode$fac>.$Fac
}
