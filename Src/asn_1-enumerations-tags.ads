Pragma Ada_2012;
Pragma Assertion_Policy( Check );

Package ASN_1.Enumerations.Tags with Pure, SPARK_Mode => On is

    Type Tag is
      (
       ASN_End_Of_Content,
       ASN_Boolean,
       ASN_Integer,
       ASN_Bit_String,
       ASN_Octet_String,
       ASN_Null,
       ASN_Object_Identifier,
       ASN_Object_Descriptor,
       ASN_External,
       ASN_Real,
       ASN_Enumerated,
       ASN_Embedded_PDV,
       ASN_UTF8_String,
       ASN_Relative_OID,
       ASN_Time,
       ASN_Reserved_For_Future_Use,
       ASN_Sequence,
       ASN_Set,
       ASN_Numeric_String,
       ASN_Printable_String,
       ASN_TeleTex_String,
       ASN_VideoTex_String,
       ASN_IA5_String,
       ASN_UTC_Time,
       ASN_Generalized_Time,
       ASN_Graphic_String,
       ASN_Visible_String,
       ASN_General_String,
       ASN_Universal_String,
       ASN_Character_String,
       ASN_BMP_String,
       ASN_Date,
       ASN_Time_Of_Day,
       ASN_Date_Time,
       ASN_Duration,
       ASN_OID_IRI, --Internationalized_Resource_Identifier,
       ASN_Relative_OID_IRI

      );

    ASN_T61_String            : Constant Tag;
    ASN_Reserved_For_Encoding : Constant Tag;

    Type Tag_Class is
      (
       Universal,
       Application,
       Context_Specific,
       Private_Use
      ) with Size => 2;

    Type Atomicity is
      (
       Primitive,
       Constructed
      ) with Size => 1;

Private

    For Tag use
      (
       ASN_End_Of_Content          => 0,
       ASN_Boolean                 => 1,
       ASN_Integer                 => 2,
       ASN_Bit_String              => 3,
       ASN_Octet_String            => 4,
       ASN_Null                    => 5,
       ASN_Object_Identifier       => 6,
       ASN_Object_Descriptor       => 7,
       ASN_External                => 8,
       ASN_Real                    => 9,
       ASN_Enumerated              => 10,
       ASN_Embedded_PDV            => 11,
       ASN_UTF8_String             => 12,
       ASN_Relative_OID            => 13,
       ASN_Time                    => 14,
       ASN_Reserved_For_Future_Use => 15,
       ASN_Sequence                => 16,
       ASN_Set                     => 17,
       ASN_Numeric_String          => 18,
       ASN_Printable_String        => 19,
       ASN_TeleTex_String          => 20,
       ASN_VideoTex_String         => 21,
       ASN_IA5_String              => 22,
       ASN_UTC_Time                => 23,
       ASN_Generalized_Time        => 24,
       ASN_Graphic_String          => 25,
       ASN_Visible_String          => 26,
       ASN_General_String          => 27,
       ASN_Universal_String        => 28,
       ASN_Character_String        => 29,
       ASN_BMP_String              => 30,
       ASN_Date                    => 31,
       ASN_Time_Of_Day             => 32,
       ASN_Date_Time               => 33,
       ASN_Duration                => 34,
       ASN_OID_IRI                 => 35,
       ASN_Relative_OID_IRI        => 36
      );

    For Tag_Class use
      (
       Universal        => 2#00#,
       Application      => 2#01#,
       Context_Specific => 2#10#,
       Private_Use      => 2#11#
      );

    For Atomicity use
      (
       Primitive        => 2#0#,
       Constructed      => 2#1#
      );


    ASN_T61_String            : Constant Tag:= ASN_TeleTex_String;
    ASN_Reserved_For_Encoding : Constant Tag:= ASN_End_Of_Content;
End ASN_1.Enumerations.Tags;
