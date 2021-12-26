with BB;       use BB;
with BB.Ideal; use BB.Ideal;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Real_Time;                     use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Characters.Latin_1;            use Ada.Characters.Latin_1;

procedure Travel_Times is
   T_0     : Time;
   T_Delta : Duration;
   Error   : Duration;

   function Get_Ideal_Travel_Time (SSO : Solar_System_Object) return Duration
   is
      type Gravities is array (Solar_System_Object) of Float;
      Gravity_Of : constant Gravities :=
        (Mercury => 3.7, Venus => 8.87, Earth => 9.80665, Mars => 3.711,
         Jupiter => 24.79, Saturn => 10.4, Uranus => 8.69, Neptune => 11.15,
         Pluto   => 0.62, Moon => 1.6249, Ceres => 0.27, Eris => 0.82,
         Vesta   => 0.22);
   begin
      return Duration (Sqrt (5.1928 / Gravity_Of (SSO)));
   end Get_Ideal_Travel_Time;
begin
   for SSO in Solar_System_Object loop
      Move_BB_To (SSO);

      Set_Beam_Angle (Max_Angle);
      while Ball_Position /= Min_Position loop
         null;
      end loop;
      T_0 := Clock;

      Set_Beam_Angle (Min_Angle);
      while Ball_Position /= Max_Position loop
         null;
      end loop;
      T_Delta := To_Duration (Clock - T_0);
      Error   := T_Delta - Get_Ideal_Travel_Time (SSO);

      Put (SSO'Image & ":");
      Put (Item => Float (T_Delta), Aft => 7, Exp => 0);
      Put (" s." & HT & "Error =");
      Put (Item => Float (Error), Aft => 7, Exp => 0);
      Put_Line (" ms.");
   end loop;
end Travel_Times;
