with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;
with Ada.Text_IO;            use Ada.Text_IO;

package body Samplers.UnitSquares is

   function Construct_UnitSquare_Regular_Sampler (No_Of_Samples : in Positive; Name : in String) return Sampler_Ptr is
      Smp_Ptr : Sampler_Ptr;
   begin
      Smp_Ptr               := new UnitSquare_Regular_Sampler;
      Smp_Ptr.Name          := To_Unbounded_String (Name);
      Smp_Ptr.No_Of_Samples := No_Of_Samples;
      return Smp_Ptr;
   end Construct_UnitSquare_Regular_Sampler;

   function Construct_UnitSquare_Random_Sampler
     (No_Of_Samples : in Positive; Min_Distance : in Large_Float; Name : in String) return Sampler_Ptr
   is
      Smp_Ptr : Sampler_Ptr;
   begin
      Smp_Ptr               := new UnitSquare_Random_Sampler;
      Smp_Ptr.Name          := To_Unbounded_String (Name);
      Smp_Ptr.No_Of_Samples := No_Of_Samples;
      Set_Minimal_Distance (UnitSquare_Random_Sampler (Smp_Ptr.all), Min_Distance);
      return Smp_Ptr;
   end Construct_UnitSquare_Random_Sampler;

   procedure Set_Minimal_Distance (Smp : in out UnitSquare_Random_Sampler; Min_Distance : in Large_Float) is
   begin
      Smp.Minimum_Distance := Min_Distance;
   end Set_Minimal_Distance;

   function Get_Minimal_Distance (Smp : in UnitSquare_Random_Sampler) return Large_Float is (Smp.Minimum_Distance);

   overriding procedure Initialize (Smp : in out UnitSquare_Regular_Sampler) is
      Samples_Per_Row, Sample : Positive;
      HdX, HdY, dX, dY        : Large_Float;
   begin
      Samples_Per_Row := Positive (Sqrt (Large_Float (Smp.No_Of_Samples)));

      dX  := 1.0 / Large_Float (Samples_Per_Row);
      HdX := 0.5 * dX;
      dY  := dX;
      HdY := HdX;

      Sample := 1;

      for Y in 1 .. Samples_Per_Row loop
         for X in 1 .. Samples_Per_Row loop
            Smp.Samples (Sample) := Construct_Point (HdX + Large_Float (X - 1) * dX, HdY + Large_Float (Y - 1) * dY);
            Sample               := Sample + 1;
         end loop;
      end loop;

   end Initialize;

   overriding procedure Initialize (Smp : in out UnitSquare_Random_Sampler) is
      Min_Dist, Smallest_Dist, Cur_Dist, SX, SY, NewX, NewY : Large_Float;
      Found                                                 : Boolean;
      Sample                                                : Integer;
   begin
      Min_Dist := Get_Minimal_Distance (Smp);

      --  Set the 1st sample position
      Sample               := 1;
      NewX                 := Large_Float (Random (USRS_Generator));
      NewY                 := Large_Float (Random (USRS_Generator));
      Smp.Samples (Sample) := Construct_Point (NewX, NewY);

      --  Loop until enough samples are generated within the minimal distance
      for X in 2 .. Smp.No_Of_Samples loop

         Smallest_Dist := 1_000.0;
         Found         := False;

         --  loop
         while not Found loop
            NewX := Large_Float (Random (USRS_Generator));
            NewY := Large_Float (Random (USRS_Generator));

            --  Check if the sample is at least mDist away from all previous samples
            for Y in 1 .. Sample loop

               SX       := Get_X (Smp.Samples (Y));
               SY       := Get_Y (Smp.Samples (Y));
               Cur_Dist := (NewX - SX) * (NewX - SX) + (NewY - SY) * (NewY - SY);
               Cur_Dist := Sqrt (Cur_Dist);
               if Cur_Dist < Smallest_Dist then
                  Smallest_Dist := Cur_Dist;
               end if;
            end loop;

            if Smallest_Dist > Min_Dist then
               Found := True;
            else
               Smallest_Dist := 1_000.0;
            end if;
         end loop;

         --  Found one, so add it to the list
         Sample               := Sample + 1;
         Smp.Samples (Sample) := Construct_Point (NewX, NewY);
      end loop;

   end Initialize;

   overriding function Get_Next_Sample (Smp : in out UnitSquare_Random_Sampler) return Point_2D is
   begin
      if Smp.Current_Sample = Smp.No_Of_Samples then
         Smp.Current_Sample := 1;
      else
         Smp.Current_Sample := Smp.Current_Sample + 1;
      end if;
      return Smp.Samples (Smp.Current_Sample);
   end Get_Next_Sample;

   overriding function Get_Next_Sample (Smp : in out UnitSquare_Regular_Sampler) return Point_2D is
   begin
      if Smp.Current_Sample = Smp.No_Of_Samples then
         Smp.Current_Sample := 1;
      else
         Smp.Current_Sample := Smp.Current_Sample + 1;
      end if;
      return Smp.Samples (Smp.Current_Sample);
   end Get_Next_Sample;

   overriding function Get_Current_Sample (Smp : in out UnitSquare_Random_Sampler) return Point_2D is
   begin
      return Smp.Samples (Smp.Current_Sample);
   end Get_Current_Sample;

   overriding function Get_Current_Sample (Smp : in out UnitSquare_Regular_Sampler) return Point_2D is
   begin
      return Smp.Samples (Smp.Current_Sample);
   end Get_Current_Sample;

end Samplers.UnitSquares;
