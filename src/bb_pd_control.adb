with BB; use BB;

package body BB_PD_Control is
   Last_Error : Float;

   Kp, Kd : Float;
   Target : Position;

   procedure Configure_PD (Kp, Kd : in Float;
                           Target : in Position := 0.0) is
   begin
      Last_Error := 0.0;
      BB_PD_Control.Kp := Kp;
      BB_PD_Control.Kd := Kd;
      Set_Target_Position(Target);
   end Configure_PD;

   procedure Set_Target_Position (Target : in Position) is
   begin
      BB_PD_Control.Target := Target;
   end Set_Target_Position;

   procedure Calculate_Action (Pos : in Position;
                               Ang : out Angle) is
      Current_Error : Float := Pos - Target;

      -- Calculate the unbounded angle. This might overflow the range of the
      -- Angle type, so we must saturate to [Min_Angle, Max_Angle]
      Unbounded_Angle : Float := Kp * Current_Error + Kd * (Current_Error - Last_Error);
   begin
      -- Saturate angle
      if Unbounded_Angle < Min_Angle then
         Ang := Min_Angle;
      elsif Unbounded_Angle > Max_Angle then
         Ang := Max_Angle;
      else
         Ang := Unbounded_Angle;
      end if;

      -- Set relative error to be used in subsequent invocations
      Last_Error := Current_Error;
   end Calculate_Action;
end BB_PD_Control;
