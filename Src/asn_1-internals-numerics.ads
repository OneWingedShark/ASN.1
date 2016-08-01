Pragma Ada_2012;
Pragma Assertion_Policy( Check );

Private With
System;

Package ASN_1.Internals.Numerics with Pure, SPARK_Mode => On is

    Type Big_Number(<>) is private;

    function "+"   (Left, Right : Big_Number) return Big_Number;
    function "-"   (Left, Right : Big_Number) return Big_Number;
    function "*"   (Left, Right : Big_Number) return Big_Number;
    function "/"   (Left, Right : Big_Number) return Big_Number;
    function "**"  (Left, Right : Big_Number) return Big_Number;
    function "MOD" (Left, Right : Big_Number) return Big_Number;
    function "REM" (Left, Right : Big_Number) return Big_Number;
    function "-"   (Left        : Big_Number) return Big_Number;
    function "ABS" (Left        : Big_Number) return Big_Number;

    function "="   (Left, Right : Big_Number) return Boolean;
    function ">="  (Left, Right : Big_Number) return Boolean;
    function "<="  (Left, Right : Big_Number) return Boolean;
    function ">"   (Left, Right : Big_Number) return Boolean;
    function "<"   (Left, Right : Big_Number) return Boolean;

    Zero,
    One   : Constant Big_Number;
Private

    Base : Constant := 2 ** System.Word_Size;
    Type Digit is range 0..Base;

    -- Represent number of words in Digit_Vector; 2**23 used to ensure
    -- that the Digit_Vector does not exceed 32MB.
    Type Length_Range is range 0 .. 2 ** 23 - 1;

    Type Digit_Vector is Array(Length_Range range <>) of Digit
    with Dynamic_Predicate => Digit_Vector'First = 1;


    Type Big_Number( Length : Length_Range ) is record
	Negetive : Boolean;
	Data     : Digit_Vector(1..Length);
    end record;

    for Big_Number use record
      Length   at 0 range 0 .. 23;
      Negetive at 3 range 0 .. 7;
   end record;

    --------------------------
    --  INTERNAL CONSTANTS  --
    --------------------------

    Zero   : Constant Big_Number :=
      (Length => 0, Negetive => False, Data => (others => 0));
    One    : Constant Big_Number :=
      (Length => 1, Negetive => False, Data => (others => 1));

    One_Data  : Digit_Vector Renames One.Data;
    Zero_Data : Digit_Vector Renames Zero.Data;

    function Convert (Item : Big_Number) return Long_Long_Integer with Inline;
    function Convert (Item : Long_Long_Integer) return Big_Number;



End ASN_1.Internals.Numerics;
