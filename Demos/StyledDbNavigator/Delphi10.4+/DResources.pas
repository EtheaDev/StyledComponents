unit DResources;

interface

uses
  System.SysUtils, System.Classes, Vcl.BaseImageCollection, Vcl.ImageCollection,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TdmResources = class(TDataModule)
    ImageCollection: TImageCollection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmResources: TdmResources;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
