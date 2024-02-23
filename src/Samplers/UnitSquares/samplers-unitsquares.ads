with Core_Types;                use Core_Types;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

--<summary>The UnitSquare_Sampler package contains the unit square sampler ADT and functions</summary>
--<description>The UnitSquare_Sampler package contains the unit square sampler ADT and functions.
--Unit square samplers are mainly used for pixels and rectangular light sampling</description>
--<group>Rendering</group>

package Samplers.UnitSquares is

   -------------------------
   -- ADT UnitSquare_Sampler
   -------------------------

   type UnitSquare_Regular_Sampler is new Sampler with private;
   --<summary>ADT of the UnitSquare Regular Sampler Sampler</summary>

   type UnitSquare_Random_Sampler is new Sampler with private;
   --<summary>ADT of the UnitSquare Random Sampler Sampler</summary>

   USRS_Generator : Generator;
   --<summary> The generator for all Unit Square Random Samplers. Can't use a local one else it generates the same sequence for each pixel.
   --</summary>

   function Construct_UnitSquare_Regular_Sampler (No_Of_Samples : in Positive; Name : in String) return Sampler_Ptr;
   --<summary>Constructs a UnitSquare Regular sampler/summary>
   --<description>Constructs a UnitSquare Regular sampler</description>
   --<parameter name="No_Of_Samples">The number of samples to generate for this sampler</parameter>
   --<parameter name="Name">The name of this sampler</parameter>
   --<exception>None at this moment</exception>

   function Construct_UnitSquare_Random_Sampler
     (No_Of_Samples : in Positive;
      Min_Distance  : in Large_Float;
      Name          : in String) return Sampler_Ptr;
   --<summary>Constructs a UnitSquare Random sampler/summary>
   --<description>Constructs a UnitSquare Random sampler</description>
   --<parameter name="No_Of_Samples">The number of samples to generate for this sampler</parameter>
   --<parameter name="Min_Distance">The minimal distance between each sample</parameter>
   --<parameter name="Name">The name of this sampler</parameter>
   --<exception>None at this moment</exception>

   procedure Initialize (Smp : in out UnitSquare_Regular_Sampler);
   --<summary>Initializes a UnitSquare_Regular_Sampler</summary>
   --<description>Initializes a UnitSquare_Regular_Sampler</description>
   --<parameter name="Smp">A Sampler of the UnitSquare_Regular_Sampler Type</parameter>
   --<exception>None at this moment</exception>

   procedure Initialize (Smp : in out UnitSquare_Random_Sampler);
   --<summary>Initializes a UnitSquare_Random_Sampler</summary>
   --<description>Initializes a UnitSquare_Random_Sampler</description>
   --<parameter name="Smp">A Sampler of the UnitSquare_Random_Sampler Type</parameter>
   --<exception>None at this moment</exception>

   function Get_Next_Sample (Smp : in out UnitSquare_Regular_Sampler) return Point_2D;
   --<summary>Get the next Point_2D for this sampler</summary>
   --<description>Get the next Point_2D for this sampler</description>
   --<parameter name="Smp">The sampler to get the next ray from</parameter>
   --<exception>None at this moment</exception>

   function Get_Next_Sample (Smp : in out UnitSquare_Random_Sampler) return Point_2D;
   --<summary>Get the next Point_2D for this sampler</summary>
   --<description>Get the next Point_2D for this sampler</description>
   --<parameter name="Smp">The sampler to get the next ray from</parameter>
   --<exception>None at this moment</exception>

   function Get_Current_Sample (Smp : in out UnitSquare_Regular_Sampler) return Point_2D;
   --<summary>Get the current Point_2D for this sampler/summary>
   --<description>Get the current Point_2D for this sampler</description>
   --<parameter name="Smp">The sampler to get the sample</parameter>
   --<exception>None at this moment</exception>>

   function Get_Current_Sample (Smp : in out UnitSquare_Random_Sampler) return Point_2D;
   --<summary>Get the current Point_3D for this sampler/summary>
   --<description>Get the current Point_3D for this sampler</description>
   --<parameter name="Smp">The sampler to get the sample</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Minimal_Distance (Smp : in out UnitSquare_Random_Sampler; Min_Distance : in Large_Float);

   function Get_Minimal_Distance (Smp : in UnitSquare_Random_Sampler) return Large_Float;

private

   type UnitSquare_Regular_Sampler is new Sampler_2D with null record;
   --<summary>Sampler to hold 2D samples distributed regularly across a unit square</summary>

   type UnitSquare_Random_Sampler is new Sampler_2D with record
      Minimum_Distance : Large_Float := 0.0;
      --  Minimum distance between each sample
   end record;
   --<summary>Sampler to hold 2D samples distributed radomly across a unit square</summary>

end Samplers.UnitSquares;
