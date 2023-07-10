unit UControlListMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.BaseImageCollection,
  SVGIconImageCollection, Vcl.VirtualImage, Vcl.ControlList, Vcl.StdCtrls,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.StyledButton,
  Vcl.StandardButtonStyles, Vcl.BootstrapButtonStyles;

type
  TControlListMainForm = class(TForm)
    ControlList: TControlList;
    SVGIconImageCollection: TSVGIconImageCollection;
    Label1: TLabel;
    VirtualImage1: TVirtualImage;
    Label2: TLabel;
    ControlListButton1: TControlListButton;
    ControlListButton2: TControlListButton;
    SVGIconImageButtons: TSVGIconImageCollection;
    VirtualImageList: TVirtualImageList;
    GraphicButton: TStyledGraphicButton;
    StyledGraphicButton1: TStyledGraphicButton;
    procedure ControlListBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure GraphicButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ControlListMainForm: TControlListMainForm;

implementation

{$R *.dfm}

procedure TControlListMainForm.ControlListBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
begin
  VirtualImage1.ImageIndex := AIndex;
  Label2.Caption := SVGIconImageCollection.SVGIconItems[AIndex].Name;
  Label1.Caption := SVGIconImageCollection.SVGIconItems[AIndex].SVGText;
  GraphicButton.Caption := SVGIconImageCollection.SVGIconItems[AIndex].Name;
  GraphicButton.Tag := AIndex;
end;

procedure TControlListMainForm.GraphicButtonClick(Sender: TObject);
var
  LButton: TStyledGraphicButton;
begin
  LButton := TStyledGraphicButton(Sender);
  ShowMessage(Format('Graphic %s Button n.%d Clicked',
    [LButton.Caption, LButton.Tag]));
end;

end.
