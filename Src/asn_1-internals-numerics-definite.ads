Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Interfaces;

Package ASN_1.Internals.Numerics.Definite with Shared_Passive, Preelaborate, SPARK_Mode => On is

    Type Number is private;


    function "+"   (Left, Right : Number) return Number;
    function "-"   (Left, Right : Number) return Number;
    function "*"   (Left, Right : Number) return Number;
    function "/"   (Left, Right : Number) return Number;
    function "**"  (Left, Right : Number) return Number;
    function "MOD" (Left, Right : Number) return Number;
    function "REM" (Left, Right : Number) return Number;
    function "-"   (Right       : Number) return Number;
    function "ABS" (Right       : Number) return Number;

    function "="   (Left, Right : Number) return Boolean;
    function ">="  (Left, Right : Number) return Boolean;
    function "<="  (Left, Right : Number) return Boolean;
    function ">"   (Left, Right : Number) return Boolean;
    function "<"   (Left, Right : Number) return Boolean;

    function Convert (Item : Number) return Long_Long_Integer;
    function Convert (Item : Long_Long_Integer) return Number;


Private
    Pragma SPARK_Mode(Off);
    use all type Big_Number;

    type Number is not null access Big_Number;

    function "+"   (Left, Right : Number) return Number is (new Big_Number'(Left.all  +  Right.all));
    function "-"   (Left, Right : Number) return Number is (new Big_Number'(Left.all  -  Right.all));
    function "*"   (Left, Right : Number) return Number is (new Big_Number'(Left.all  *  Right.all));
    function "/"   (Left, Right : Number) return Number is (new Big_Number'(Left.all  /  Right.all));
    function "**"  (Left, Right : Number) return Number is (new Big_Number'(Left.all **  Right.all));
    function "MOD" (Left, Right : Number) return Number is (new Big_Number'(Left.all Mod Right.all));
    function "REM" (Left, Right : Number) return Number is (new Big_Number'(Left.all Rem Right.all));
    function "-"   (Right       : Number) return Number is (new Big_Number'(-Right.all));
    function "ABS" (Right       : Number) return Number is (new Big_Number'(Abs Right.all));

    function "="   (Left, Right : Number) return Boolean is (Left.all  = Right.all);
    function ">="  (Left, Right : Number) return Boolean is (Left.all >= Right.all);
    function "<="  (Left, Right : Number) return Boolean is (Left.all <= Right.all);
    function ">"   (Left, Right : Number) return Boolean is (Left.all  > Right.all);
    function "<"   (Left, Right : Number) return Boolean is (Left.all  < Right.all);

    function Convert (Item : Number) return Long_Long_Integer is (Convert(Item.all));
    function Convert (Item : Long_Long_Integer) return Number is (new Big_Number'(Convert(Item)));

End ASN_1.Internals.Numerics.Definite;
