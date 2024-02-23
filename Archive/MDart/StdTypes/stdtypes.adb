--  package StdTypes body

with Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Long_Long_Float_Text_IO;

package body StdTypes is

   procedure Put (X : in Integer_16) is
   begin
      Ada.Integer_Text_IO.Put (Integer (X));
   end Put;

   procedure Put (X : in Integer_32) is
   begin
      Ada.Integer_Text_IO.Put (Integer (X));
   end Put;

   procedure Put (X : in Integer_64) is
   begin
      Ada.Long_Long_Integer_Text_IO.Put (Long_Long_Integer (X));
   end Put;

   procedure Put (X : in Float_32) is
   begin
      Ada.Float_Text_IO.Put (Float (X));
   end Put;

   procedure Put (X : in Float_64) is
   begin
      Ada.Long_Float_Text_IO.Put (Long_Float (X));
   end Put;

   procedure Put (X : in Float_96) is
   begin
      Ada.Long_Long_Float_Text_IO.Put (Long_Long_Float (X));
   end Put;

end StdTypes;
