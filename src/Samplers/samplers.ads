with Linear_Math;           use Linear_Math;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--<summary>The Samplers package contains the main sampler ADT and functions</summary>
--<description>The Samplers package contains the main sampler ADT and functions. Samplers are used to distribute
--rays according to some sampling distribution. Specific samplers are defined in sub-packages</description>
--<group>Rendering</group>

package Samplers is

   --------------
   -- ADT Sampler
   --------------

   MAX_NO_OF_SAMPLES : constant Positive := 1024;
   --<summary>Maximum number of samples for any sampler. Stored in the public part, as other packages may use it.</summary>

   type Sampler is tagged private;
   --<summary>ADT of the root type of all Sampler objects</summary>

   type Sampler_Ptr is access all Sampler'Class;
   --<summary>The access type to all Sampler'Class objects</summary

   procedure Put (Smp : in Sampler'Class; Msg : in String := "Sampler = ");
   --<summary>Prints a Sampler's statistics</summary>
   --<description>Prints a Sampler's statistics</description>
   --<parameter name="Smp">A Sampler of the Sampler Class Wide Type</parameter>
   --<exception>None at this moment</exception>

   procedure Initialize (Smp : in out Sampler);
   --<summary>Initializes a Sampler</summary>
   --<description>Initializes a Sampler</description>
   --<parameter name="Smp">A Sampler of the Sampler Class Wide Type</parameter>
   --<exception>None at this moment</exception>

   function Get_No_Of_Samples (Smp : in Sampler) return Natural;
   --<summary>Get the number of samples for this sampler/summary>
   --<description>Get the number of samples for this sampler</description>
   --<parameter name="Smp">The sampler to get the number of samples from</parameter>
   --<exception>None at this moment</exception>

   function Get_Next_Sample (Smp : in out Sampler) return Point_2D;
   --<summary>Get the next Point_2D for this sampler/summary>
   --<description>Get the next Point_2D for this sampler</description>
   --<parameter name="Smp">The sampler to get the next ray from</parameter>
   --<exception>None at this moment</exception>

   function Get_Next_Sample (Smp : in out Sampler) return Point_3D;
   --<summary>Get the next Point_2D for this sampler/summary>
   --<description>Get the next Point_2D for this sampler</description>
   --<parameter name="Smp">The sampler to get the next ray from</parameter>
   --<exception>None at this moment</exception>

   function Get_Current_Sample (Smp : in out Sampler) return Point_2D;
   --<summary>Get the current Point_2D for this sampler/summary>
   --<description>Get the current Point_2D for this sampler</description>
   --<parameter name="Smp">The sampler to get the sample</parameter>
   --<exception>None at this moment</exception>

   function Get_Current_Sample (Smp : in out Sampler) return Point_3D;
   --<summary>Get the current Point_3D for this sampler/summary>
   --<description>Get the current Point_3D for this sampler</description>
   --<parameter name="Smp">The sampler to get the sample</parameter>
   --<exception>None at this moment</exception>

private

   type Sample_Array_2D is array (1 .. MAX_NO_OF_SAMPLES) of Point_2D;
   --<summary>Data type to hold a series of samples as Point_2D</summary>

   type Sample_Array_3D is array (1 .. MAX_NO_OF_SAMPLES) of Point_3D;
   --<summary>Data type to hold a series of samples as Point_3D</summary>

   type Sampler is tagged record
      Name : Unbounded_String := To_Unbounded_String ("NOT DEFINED");
      --  The Name of the Sampler;
      No_Of_Samples : Positive := 1;
      --  The number of samples defined for this Sampler;
      Current_Sample : Natural := 0;
      --  Last sample that has been returned
   end record;

   type Sampler_2D is new Sampler with record
      Samples : Sample_Array_2D;
      --  The samples for this sampler
   end record;

   type Sampler_3D is new Sampler with record
      Samples : Sample_Array_3D;
      --  The samples for this sampler
   end record;
end Samplers;
