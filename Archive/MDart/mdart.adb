--  Package MDart body
with Ada.Text_IO;
with Ada.Calendar;
use Ada.Calendar;
with Ada.Calendar.Formatting;

package body MDart is
   startTime  : Ada.Calendar.Time;
   endTime    : Ada.Calendar.Time;
   runTime    : Duration;

   procedure Opening_Message is
   begin
      startTime := Ada.Calendar.Clock;
      Ada.Text_IO.New_Line (1);
      Ada.Text_IO.Put_Line ("M-Dart, version " & M_Dart_Version);
      Ada.Text_IO.New_Line (1);
      Ada.Text_IO.Put ("Program start at: ");
      Ada.Text_IO.Put_Line (Ada.Calendar.Formatting.Image (startTime, True, 120));
      Ada.Text_IO.New_Line (1);
   end Opening_Message;

   procedure Closing_Message is
   begin
      endTime    := Ada.Calendar.Clock;
      runTime    := endTime - startTime;
      Ada.Text_IO.New_Line (1);
      Ada.Text_IO.Put ("Program runtime: ");
      Ada.Text_IO.Put_Line (Ada.Calendar.Formatting.Image (runTime, True));
      Ada.Text_IO.New_Line (1);
   end Closing_Message;

end MDart;
