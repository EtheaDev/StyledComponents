program StyledButtonsDemo;

uses
  Vcl.Forms,
  MainFormUnit in '..\MainFormUnit.pas' {MainForm},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledButton in '..\..\..\source\Vcl.StyledButton.pas',
  Vcl.ButtonStylesAttributes in '..\..\..\source\Vcl.ButtonStylesAttributes.pas',
  Vcl.BootstrapButtonStyles in '..\..\..\source\Vcl.BootstrapButtonStyles.pas',
  Vcl.AngularButtonStyles in '..\..\..\source\Vcl.AngularButtonStyles.pas',
  Vcl.StandardButtonStyles in '..\..\..\source\Vcl.StandardButtonStyles.pas',
  Vcl.StyledButtonEditorUnit in '..\..\..\packages\Vcl.StyledButtonEditorUnit.pas' {StyledButtonEditor},
  DResources in '..\DResources.pas' {dmResources: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
