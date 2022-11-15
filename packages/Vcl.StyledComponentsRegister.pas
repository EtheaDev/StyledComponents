{******************************************************************************}
{                                                                              }
{       StyledComponents: a set of Styled VCL Component                        }
{                                                                              }
{       Copyright (c) 2022 (Ethea S.r.l.)                                      }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
{                                                                              }
{******************************************************************************}
unit Vcl.StyledComponentsRegister;

interface

uses
  Classes
  , DesignIntf
  , DesignEditors
  , VCLEditors
  , Vcl.StyledButton
  ;

Type
  TButtonFamilyPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TButtonPropertyEditor = class(TPropertyEditor)
  protected
    function GetButton: TStyledGraphicButton;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override; abstract;
  end;

  TButtonStyleClassPropertyEditor = class(TButtonPropertyEditor)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TButtonStyleAppearancePropertyEditor = class(TButtonPropertyEditor)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TStyledGraphicButtonComponentEditor = class (TComponentEditor)
  private
    function GetButton: TStyledGraphicButton;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TStyledButtonsSelection = class (TSelectionEditor, ISelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Vcl.StyledTaskDialog
  , Vcl.ButtonStylesAttributes
  , Vcl.StandardButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.StyledButtonEditorUnit
  , Vcl.StyledCmpStrUtils
  , System.Contnrs
  , Winapi.ShellAPI
  , Winapi.Windows
  ;

{ TButtonFamilyPropertyEditor }

function TButtonFamilyPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TButtonFamilyPropertyEditor.GetValues(Proc: TGetStrProc);
var
  LFamilies: TObjectList;
  I: Integer;
begin
  LFamilies := GetButtonFamilies;
  if Assigned(LFamilies) then
  begin
    for I := 0 to LFamilies.Count -1 do
      Proc(GetButtonFamilyName(I));
  end;
end;

{ TStyledGraphicButtonComponentEditor }

function TStyledGraphicButtonComponentEditor.GetButton: TStyledGraphicButton;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent;
  if LComponent is TStyledGraphicButton then
    Result := TStyledGraphicButton(LComponent);
end;

procedure TStyledGraphicButtonComponentEditor.Edit;
begin
  inherited;
end;

procedure TStyledGraphicButtonComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditStyledButton(GetButton) then
      Designer.Modified;
  end
  else if Index = 1 then
  ShellExecute(0, 'open',
    PChar(GetProjectURL), nil, nil, SW_SHOWNORMAL);
end;

function TStyledGraphicButtonComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Styled Button Editor...'
  else if Index = 1 then
    Result := 'Project page on GitHub...';
end;

function TStyledGraphicButtonComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TButtonPropertyEditor }

procedure TButtonPropertyEditor.Edit;
var
  Component : TComponent;
begin
  Component := TComponent(GetComponent(0));
  if Component is TStyledGraphicButton then
  begin
    if EditStyledButton(TStyledGraphicButton(Component)) then
      Designer.Modified;
  end;
end;

function TButtonPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TButtonPropertyEditor.GetButton: TStyledGraphicButton;
var
  Component : TComponent;
begin
  Component := TComponent(GetComponent(0));
  if Component is TStyledGraphicButton then
    Result := TStyledGraphicButton(Component)
  else
    Result := nil;
end;

{ TButtonStyleClassPropertyEditor }

function TButtonStyleClassPropertyEditor.GetValue: string;
var
  LButton: TStyledGraphicButton;
begin
  LButton := GetButton;
  if Assigned(LButton) then
    Result := LButton.StyleClass;
end;

procedure TButtonStyleClassPropertyEditor.SetValue(const Value: string);
var
  LButton: TStyledGraphicButton;
begin
  LButton := GetButton;
  if Assigned(LButton) then
  begin
    LButton.StyleClass := Value;
    Designer.Modified;
  end;
end;

{ TButtonStyleAppearancePropertyEditor }

function TButtonStyleAppearancePropertyEditor.GetValue: string;
var
  LButton: TStyledGraphicButton;
begin
  LButton := GetButton;
  if Assigned(LButton) then
    Result := LButton.StyleAppearance;
end;

procedure TButtonStyleAppearancePropertyEditor.SetValue(const Value: string);
var
  LButton: TStyledGraphicButton;
begin
  LButton := GetButton;
  if Assigned(LButton) then
  begin
    LButton.StyleAppearance := Value;
    Designer.Modified;
  end;
end;

{ TStyledButtonsSelection }
procedure TStyledButtonsSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  //Units added to source to manage styles of buttons
  proc('Vcl.StandardButtonStyles');
  proc('Vcl.BootstrapButtonStyles');
end;

procedure Register;
begin
  RegisterComponents('Styled Components',
    [TStyledGraphicButton,
     TStyledButton,
     TStyledTaskDialog]);

  RegisterPropertyEditor(TypeInfo(TStyledButtonFamily),
    TStyledGraphicButton, 'StyleFamily', TButtonFamilyPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledGraphicButton, 'StyleClass', TButtonStyleClassPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStyledButtonClass),
    TStyledGraphicButton, 'StyleAppearance', TButtonStyleAppearancePropertyEditor);

  RegisterComponentEditor(TStyledGraphicButton,
    TStyledGraphicButtonComponentEditor);

  //To auto add units
  RegisterSelectionEditor(TStyledGraphicButton, TStyledButtonsSelection);
end;

end.
