program StyledButtonsDemo;

uses
  Vcl.Forms,
  MainFormUnitOld in '..\MainFormUnitOld.pas' {MainForm},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledButton in '..\..\..\source\Vcl.StyledButton.pas',
  Vcl.BootstrapButtonStyles in '..\..\..\source\Vcl.BootstrapButtonStyles.pas',
  Vcl.AngularButtonStyles in '..\..\..\source\Vcl.AngularButtonStyles.pas',
  Vcl.StandardButtonStyles in '..\..\..\source\Vcl.StandardButtonStyles.pas',
  Vcl.StyledButtonEditorUnit in '..\..\..\packages\Vcl.StyledButtonEditorUnit.pas' {StyledButtonEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
