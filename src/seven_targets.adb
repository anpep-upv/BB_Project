with System;
with BB; use BB;
with BB.Ideal; use BB.Ideal;
with BB.GUI; use BB.GUI;
with BB_PD_Control; use BB_PD_Control;
with Ada.Real_Time; use Ada.Real_Time;

procedure Seven_Targets is
   Targets : constant array (1 .. 7) of Position :=
     (1 => Max_Position / 4.0,  2 => Min_Position / 4.0,
      3 => Max_Position / 2.0,  4 => Min_Position / 2.0,
      5 => Max_Position - 10.0, 6 => Min_Position + 10.0, 7 => 0.0);
   
   task Controller
     with Priority => System.Priority'Last - 5
   is
      entry Start;
      entry Finish;
   end Controller;
   
   task body Controller is
      Period : Time_Span := Microseconds(62500);
      Next : Time := Clock;
      
      Ang : Angle;
   begin
      accept Start;
      loop
         select
            accept Finish;
            exit;
         or
            delay until Next;
            
            Calculate_Action(Ball_Position, Ang);
            Set_Beam_Angle(Ang);
            
            Next := Next + Period;
         end select;
      end loop;
   end Controller;
begin
   Configure_PD(0.2, 0.35);
   Controller.Start;
   
   for I in Targets'Range loop
      Set_Target_Position(Targets(I));
      GUI_Setpoint(Targets(I));
      delay 6.0;
   end loop;
   
   Controller.Finish;
end Seven_Targets;
