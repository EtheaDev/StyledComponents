unit StyledTaskDialogAnimCustomForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Skia.Vcl.StyledTaskDialogAnimatedUnit,
  System.Skia, Vcl.ButtonStylesAttributes, Vcl.StyledButton, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Skia;

type
  TfmStyledTaskDialogAnimCustom = class(TStyledTaskDialogAnimatedForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
  Vcl.StyledTaskDialog
  , Vcl.StyledTaskDialogFormUnit
  ;

procedure TfmStyledTaskDialogAnimCustom.FormCreate(Sender: TObject);
begin
  inherited;
  //Custom: Show Icon for Animated Dialogs in Right position
  ImagePanel.Align := alRight;
end;

initialization
  RegisterTaskDialogFormClass(TfmStyledTaskDialogAnimCustom);
  InitializeStyledTaskDialogs(True);

finalization
  UnregisterTaskDialogFormClass(TfmStyledTaskDialogAnimCustom);

end.
