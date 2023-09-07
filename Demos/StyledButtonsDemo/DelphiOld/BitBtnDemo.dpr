program BitBtnDemo;

uses
  Vcl.Forms,
  MainFormBitBtn in '..\MainFormBitBtn.pas' {fmBitBtn};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'BitButtons - Kind and ModalResult Demo - (c) Copyright Ethea S.r.l.';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmBitBtn, fmBitBtn);
  Application.Run;
end.
