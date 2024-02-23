with Tracers;                use Tracers;
with Utilities;              use Utilities;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Tags;               use Ada.Tags;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Materials is

   ---------------
   -- ADT Material
   ---------------

   procedure Put (Mat : in Material'Class; Msg : in String := "Material = ") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put ("Class = ");
      Put (External_Tag (Mat'Tag));
      New_Line;
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Mat.Name));
      New_Line;
      Put_Line ("END " & Msg);
   end Put;

   function Max_Weight_Spectrum (Weight : in Small_Float; Factor : in Small_Float; Spectrum : RGB_Spectrum) return Small_Float is
      Max_RGB : Small_Float := 0.0;
   begin
      Max_RGB := Get_R (Spectrum);
      if Get_G (Spectrum) > Max_RGB then
         Max_RGB := Get_G (Spectrum);
      elsif Get_B (Spectrum) > Max_RGB then
         Max_RGB := Get_B (Spectrum);
      end if;
      return Weight * Factor * Max_RGB;
   end Max_Weight_Spectrum;

   function BDRF (Mat : in Material; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum is
      Spec : RGB_Spectrum;
   begin

      Debug_Message ("*** Warning: Calling a Root BDRF: Material Name = " & To_String (Mat.Name));
      return Spec;
   end BDRF;

   function BDTF (Mat : in Material; In_Ray : in Ray; Hp : in HitPoint; Weight : in Small_Float) return RGB_Spectrum is
      Spec : RGB_Spectrum;
   begin
      Debug_Message ("*** Warning: Calling a Root BDTF: Material Name = " & To_String (Mat.Name));
      return Spec;
   end BDTF;

end Materials;
