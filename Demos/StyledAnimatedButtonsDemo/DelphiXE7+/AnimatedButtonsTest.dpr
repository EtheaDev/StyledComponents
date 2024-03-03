program AnimatedButtonsTest;

uses
  Vcl.Forms,
  MainAnimatedButtonsForm in '..\MainAnimatedButtonsForm.pas' {TestForm},
  Vcl.Themes,
  Vcl.Styles,
  Skia.Vcl.StyledTaskDialogAnimatedUnit in '..\..\..\source\Skia.Vcl.StyledTaskDialogAnimatedUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Animated Buttons Demo - (c) Ethea S.r.l.';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.
