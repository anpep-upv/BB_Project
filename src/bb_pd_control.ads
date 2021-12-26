with BB; use BB;

package BB_PD_Control is

   procedure Configure_PD (Kp, Kd : in Float;
                           Target : in Position := 0.0);

   procedure Set_Target_Position (Target : in Position);

   procedure Calculate_Action (Pos : in Position;
                               Ang : out Angle);

end BB_PD_Control;
