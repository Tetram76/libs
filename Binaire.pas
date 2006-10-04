unit Binaire;

interface

Const
 Bit0 = 1;
 Bit1 = 2;
 Bit2 = 4;
 Bit3 = 8;
 Bit4 = 16;
 Bit5 = 32;
 Bit6 = 64;
 Bit7 = 128;

 Bit8 = 256;
 Bit9 = 512;
 Bit10 = 1024;
 Bit11 = 2048;
 Bit12 = 4096;
 Bit13 = 8192;
 Bit14 = 16384;
 Bit15 = 32768;

Procedure SetBit(var SetWord : Word; BitNum : Word);
Procedure ClearBit(var SetWord : Word; BitNum : Word);
Procedure ToggleBit(var SetWord : Word; BitNum : Word);
Function GetBitStat(SetWord, BitNum : Word) : Boolean;

implementation

Procedure SetBit(var SetWord : Word; BitNum : Word);
Begin
  SetWord := SetWord Or BitNum;     { Set bit }
End;

Procedure ClearBit(var SetWord : Word; BitNum : Word);
Begin
  SetWord := SetWord Or BitNum;     { Set bit    }
  SetWord := SetWord Xor BitNum;    { Toggle bit }
End;

Procedure ToggleBit(var SetWord : Word; BitNum : Word);
Begin
  SetWord := SetWord Xor BitNum;    { Toggle bit }
End;

Function GetBitStat(SetWord, BitNum : Word) : Boolean;
Begin
  Result := (SetWord And BitNum) = BitNum;            { If bit is set }
End;

end.
 