program StyledButtonInControlList;

uses
  Vcl.Forms,
  UControlListMain in 'UControlListMain.pas' {ControlListMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TControlListMainForm, ControlListMainForm);
  Application.Run;
end.
