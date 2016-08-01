Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Interfaces;

Package ASN_1.Types with Pure, SPARK_Mode => On is

    Type Octet_String is Array(Positive range <>) of Interfaces.Unsigned_8;

    Type Bits_5 is range 0..2**5-1 with Size => 5;
    Type Bits_7 is range 0..2**7-1 with Size => 7;

End ASN_1.Types;
