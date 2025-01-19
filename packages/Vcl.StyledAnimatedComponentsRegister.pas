{******************************************************************************}
{                                                                              }
{  StyledAnimatedComponents: a set of Styled VCL Component                     }
{  with animations                                                             }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
{                                                                              }
{******************************************************************************}
unit Vcl.StyledAnimatedComponentsRegister;

interface

uses
  Classes
  , System.SysUtils
  , DesignIntf
  , Designer
  , DesignEditors
  , VCLEditors
  , Vcl.Controls
  , Vcl.ComCtrls
  , Vcl.StyledAnimatedButton
  , Vcl.StyledAnimatedToolbar
  ;

Type
  TStyledAnimatedButtonComponentEditor = class (TComponentEditor)
  private
    function GetButton: TControl;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TStyledAnimatedToolbarComponentEditor = class (TComponentEditor)
  private
    function GetToolbar: TStyledAnimatedToolbar;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

(*
  TStyledAnimatedDbNavigatorComponentEditor = class (TComponentEditor)
  private
    function GetDbNavigator: TStyledAnimatedDbNavigator;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
*)

  { TSkAnimatedImageSourcePropertyEditor }

  TAnimatedImageSourcePropertyEditor = class(TPropertyEditor)
  protected
    function GetIsDefault: Boolean; override;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    class function TryEdit(var AData: TBytes): Boolean; static;
  end;

procedure Register;

implementation

uses
  Vcl.Forms
  , Vcl.StyledAnimatedTaskDialog
  , Vcl.StyledButton
  , Vcl.StyledToolbar
  , Vcl.StyledButtonEditorUnit
  , Vcl.StyledCmpStrUtils
  , Vcl.DbCtrls
  , System.Contnrs
  , Winapi.ShellAPI
  , Winapi.Windows
  , Vcl.Skia
  , Vcl.Skia.Designtime
  , Vcl.Skia.Designtime.Editor.AnimatedImage
  ;


{ TStyledAnimatedButtonComponentEditor }

function TStyledAnimatedButtonComponentEditor.GetButton: TControl;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledAnimatedButton then
    Result := TStyledAnimatedButton(LComponent);
end;

procedure TStyledAnimatedButtonComponentEditor.Edit;
begin
  inherited;
end;

procedure TStyledAnimatedButtonComponentEditor.ExecuteVerb(Index: Integer);
var
  LButton: TStyledAnimatedButton;
  LSource: TBytes;
begin
  inherited;
  LButton := GetButton as TStyledAnimatedButton;
  if Index = 0 then
  begin
    if EditStyledButton(LButton) then
      Designer.Modified;
  end
  else if Index = 1 then
  begin
    LSource := LButton.AnimationSource.Data;
    if TAnimatedImageSourcePropertyEditor.TryEdit(LSource) then
      LButton.AnimationSource.Data := LSource;
  end
  else if Index = 2 then
  begin
    ShellExecute(0, 'open',
      PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
  end;
end;

function TStyledAnimatedButtonComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled Button Editor...'
  else if Index = 1 then
    Result := 'Animation Editor...'
  else if Index = 2 then
    Result := 'Project page on GitHub...';
end;

function TStyledAnimatedButtonComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TStyledAnimatedToolbarComponentEditor }

procedure TStyledAnimatedToolbarComponentEditor.ExecuteVerb(Index: Integer);
var
  LToolbar: TStyledToolbar;
  LNewButton: TStyledToolButton;
begin
  inherited;
  if Index = 0 then //Show Styled Toolbar Editor...
  begin
    LToolbar := GetToolbar;
    LNewButton := TStyledAnimatedToolButton.CreateStyled(LToolbar.Owner,
      LToolbar.StyleFamily, LToolbar.StyleClass, LToolbar.StyleAppearance);
    try
      LNewButton.StyleDrawType := LToolbar.StyleDrawType;
      LNewButton.StyleRadius := LToolbar.StyleRadius;
      LNewButton.SetBounds(0, 0, LToolbar.ButtonWidth, LToolbar.ButtonHeight);
      if LToolbar.ShowCaptions then
      begin
        LNewButton.Name := Designer.UniqueName('Button');
        LNewButton.Caption := 'Button';
      end;
      if EditStyledButton(LNewButton) then
      begin
        LToolbar.SetToolbarStyle(LNewButton.StyleFamily,
          LNewButton.StyleClass, LNewButton.StyleAppearance);
        LToolbar.StyleRadius := LNewButton.StyleRadius;
        LToolbar.StyleDrawType := LNewButton.StyleDrawType;
        Designer.Modified;
      end;
    finally
      LNewButton.Free;
    end;
  end
  else if Index = 1 then //Add a StyledAnimatedToolButton
  begin
    LToolbar := GetToolbar;
    if LToolbar.NewButton(LNewButton, tbsButton) then
    begin
      //Set unique Name to the Button created
      LNewButton.Name := Designer.UniqueName('StyledAnimatedToolButton');
      LNewButton.Caption := LNewButton.Name;

      //Select the button into Designer
      Designer.SelectComponent(LNewButton);
      Designer.Modified;
    end;
  end
  else if Index = 2 then //Add a StyledAnymatedToolButton as Separator
  begin
    if GetToolbar.NewButton(LNewButton, tbsSeparator) then
    begin
      //Set unique Name to the Button created
      LNewButton.Name := Designer.UniqueName('StyledAnimatedToolButton');
      LNewButton.Caption := LNewButton.Name;
      //Select the button into Designer
      Designer.SelectComponent(LNewButton);
      Designer.Modified;
    end;
  end
  else if Index = 3 then //Open Project page on GitHub...
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledAnimatedToolbarComponentEditor.GetToolbar: TStyledAnimatedToolbar;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledAnimatedToolbar then
    Result := TStyledAnimatedToolbar(LComponent);
end;

function TStyledAnimatedToolbarComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled Animated Toolbar Editor...'
  else if Index = 1 then
    Result := 'Add StyledAnimatedToolbar Button'
  else if Index = 2 then
    Result := 'Add StyledAnimatedToolbar Separator'
  else if Index = 3 then
    Result := 'Project page on GitHub...';
end;

function TStyledAnimatedToolbarComponentEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

(*
{ TStyledAnimatedDbNavigatorComponentEditor }

procedure TStyledAnimatedDbNavigatorComponentEditor.ExecuteVerb(Index: Integer);
var
  LDbNavigator: TStyledAnimatedDbNavigator;
  LNavButton: TStyledAnimatedNavButton;
begin
  inherited;
  if Index = 0 then
  begin
    LDbNavigator := GetDbNavigator;
    LNavButton := LDbNavigator.Buttons[nbFirst];
    LNavButton.StyleDrawType := LDbNavigator.StyleDrawType;
    LNavButton.StyleRadius := LDbNavigator.StyleRadius;
    LNavButton.SetBounds(0, 0, LDbNavigator.ButtonWidth, LDbNavigator.ButtonHeight);
    if LDbNavigator.ShowCaptions then
    begin
      LNavButton.Name := Designer.UniqueName('Button');
      LNavButton.Caption := 'Button';
    end;
    if EdiTStyledAnimatedButton(LNavButton) then
    begin
      LDbNavigator.SetDbNavigatorStyle(LNavButton.StyleFamily,
        LNavButton.StyleClass, LNavButton.StyleAppearance);
      LDbNavigator.StyleRadius := LNavButton.StyleRadius;
      LDbNavigator.StyleDrawType := LNavButton.StyleDrawType;
      Designer.Modified;
    end;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledAnimatedDbNavigatorComponentEditor.GetDbNavigator: TStyledAnimatedDbNavigator;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledAnimatedDbNavigator then
    Result := TStyledAnimatedDbNavigator(LComponent);
end;

function TStyledAnimatedDbNavigatorComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled DbNavigator Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledAnimatedDbNavigatorComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;
*)

{ TAnimatedImageSourcePropertyEditor }

function TAnimatedImageSourcePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  inherited;
  Result := [paDialog];
end;

function TAnimatedImageSourcePropertyEditor.GetIsDefault: Boolean;
var
  LComponent: TPersistent;
begin
  LComponent := GetComponent(0);
  if LComponent is TStyledAnimatedButton then
    Result := TStyledAnimatedButton(LComponent).AnimationSource.Data <> nil
  else
    Result := False;
end;

procedure TAnimatedImageSourcePropertyEditor.Edit;
var
  LData: TBytes;
  LComponent: TPersistent;
begin
  LComponent := GetComponent(0);
  if LComponent is TStyledAnimatedButton then
  begin
    LData := TStyledAnimatedButton(LComponent).AnimationSource.Data;
    TStyledAnimatedButton(LComponent).AnimatedImage.Visible := False;
    if TryEdit(LData) then
    begin
      TStyledAnimatedButton(LComponent).AnimationSource.Data := LData;
      Modified;
      TStyledAnimatedButton(LComponent).AnimatedImage.Visible := True;
    end;
  end;
end;

function TAnimatedImageSourcePropertyEditor.GetValue: string;
begin
  Result := 'TSkAnimatedImage.TSource';
end;

class function TAnimatedImageSourcePropertyEditor.TryEdit(
  var AData: TBytes): Boolean;
var
  LAnimatedImageEditorForm: TSkAnimatedImageEditorForm;
begin
  LAnimatedImageEditorForm := TSkAnimatedImageEditorForm.Create(Application);
  try
    Result := LAnimatedImageEditorForm.ShowModal(AData) = mrOk;
  finally
    LAnimatedImageEditorForm.Free;
  end;
end;

procedure Register;
begin
  Classes.RegisterClass(TStyledAnimatedToolButton);

  RegisterComponents('Styled Animated Components',
    [TStyledAnimatedButton
    //,TStyledAnimatedToolbar
    ,TStyledAnimatedTaskDialog
    //,TStyledAnimatedDbNavigator
    ]);

  RegisterPropertyEditor(TypeInfo(TSkAnimatedImage.TSource),
    TStyledAnimatedButton, 'AnimationSource', TAnimatedImageSourcePropertyEditor);

  RegisterComponentEditor(TStyledAnimatedButton, TStyledAnimatedButtonComponentEditor);
  RegisterComponentEditor(TStyledAnimatedToolButton, TStyledAnimatedButtonComponentEditor);
  RegisterComponentEditor(TStyledAnimatedToolbar, TStyledAnimatedToolbarComponentEditor);
  //RegisterComponentEditor(TStyledAnimatedDbNavigator, TStyledAnimatedDbNavigatorComponentEditor);
end;

end.
