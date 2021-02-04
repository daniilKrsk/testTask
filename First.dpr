program Project1;

{$APPTYPE CONSOLE}

{

1. Есть некоторый класс TParent = class и классы TСhild1=class(TParent) и
TСhild2=class(TParent). У TChild1 и TChild2 есть некоторые published свойства
(например PropA и PropB). Необходимо реализовать возможность сохранения и
восстановления published свойств (например PropA и PropB) классов TСhild1 и
TСhild2 методами TParent в реестре, ini файле или текстовом файле.
Результатом является pas файл с реализацией классов.
}

uses Contnrs, SysUtils, TypInfo, IniFiles, Classes, Registry, Windows, Variants;

type
  TCustomContainer = class
  public
    procedure SaveProperty(Sender: TObject; AName: string; AValue: string); virtual;
    procedure LoadProperty(Sender: TObject; AName: string; var AValue: string); virtual;
    constructor Create(AContainer: string); virtual;
  end;

  TIniContainer = class(TCustomContainer)
  private
    FIniFile: TIniFile;
  public
    procedure SaveProperty(Sender: TObject; AName: string; AValue: string); override;
    procedure LoadProperty(Sender: TObject; AName: string; var AValue: string); override;
    constructor Create(AContainer: string); override;
    destructor Destroy; override;
  end;
  
  TRegContainer = class(TCustomContainer)
  private
    FRegistry: TRegistry;  
  public
    procedure SaveProperty(Sender: TObject; AName: string; AValue: string); override;
    procedure LoadProperty(Sender: TObject; AName: string; var AValue: string); override;
    constructor Create(AContainer: string); override;
    destructor Destroy; override;
  end;

  TTxtContainer = class(TCustomContainer)
  private
    FContainer: string;
    FStringList: TStringList;
  public
    procedure SaveProperty(Sender: TObject; AName: string; AValue: string); override;
    procedure LoadProperty(Sender: TObject; AName: string; var AValue: string); override;
    constructor Create(AContainer: string); override;
    destructor Destroy; override;
  end;

  TParent = class(TPersistent)
  private
    Fcontainer: TCustomContainer;
    procedure Setcontainer(const Value: TCustomContainer);
  protected
    procedure SaveValue(AName, AValue: string);
    function LoadValue(AName: string): string;
  public
    procedure Clear;
    procedure Load;
    procedure Save;
    constructor create; virtual;
    property container: TCustomContainer read Fcontainer write Setcontainer;
  end;

  TChild1 = class(TParent)
  private
    FPropA: string;
    procedure SetPropA(const Value: string);
  public
    constructor create; override;
  published
    property PropA: string read FPropA write SetPropA;
  end;

  TChild2 = class(TParent)
  private
    FPropB: double;
    procedure SetPropB(const Value: double);
  public
    constructor create; override;
  published
    property PropB: double read FPropB write SetPropB;
  end;

{ TChild2 }

constructor TChild2.create;
begin
  inherited;
  PropB := 100;
end;

procedure TChild2.SetPropB(const Value: double);
begin
  FPropB := Value;
end;

{ TParent }

procedure TParent.Clear;
var
  PropList: PPropList;
  i: integer;
begin
  for i := 0 to GetPropList(self, PropList) - 1 do
    case (PropList[i].PropType^.Kind) of
      tkFloat:
        SetFloatProp(self, PropList[i].Name, 0);
      else
        SetPropValue(self, PropList[i].Name, Null);
    end;
end;

constructor TParent.create;
begin
end;

procedure TParent.Load;
var
  PropList: PPropList;
  Value: string;
  i: integer;
begin
  for i := 0 to GetPropList(self, PropList) - 1 do
  begin
    Value := LoadValue(PropList[i].Name);
    case (PropList[i].PropType^.Kind) of
      tkFloat:
      begin
        if(Value = '') then
          SetFloatProp(self, PropList[i].Name, 0)
        else
          SetFloatProp(self, PropList[i].Name, StrToFloat(Value));
      end
      else
          SetPropValue(self, PropList[i].Name, Value);
    end;
  end;
end;

function TParent.LoadValue(AName: string): string;
begin
  if Assigned(FContainer) then
    FContainer.LoadProperty(self, AName, Result);
end;

procedure TParent.Save;
var
  PropList: PPropList;
  i: integer;
begin
  for i := 0 to GetPropList(self, PropList) - 1 do
    SaveValue(PropList[i].Name, GetPropValue(self, PropList[i].Name, True));
end;

procedure TParent.SaveValue(AName, AValue: string);
begin
  if Assigned(FContainer) then
    FContainer.SaveProperty(self, AName, AValue);
end;

procedure TParent.Setcontainer(const Value: TCustomContainer);
begin
  Fcontainer := Value;
end;

{ TChild1 }

constructor TChild1.create;
begin
  inherited;
  PropA := 'string';
end;

procedure TChild1.SetPropA(const Value: string);
begin
  FPropA := Value;
end;

{ TCustomContainer }

constructor TCustomContainer.Create(AContainer: string);
begin
  //
end;

procedure TCustomContainer.LoadProperty(Sender: TObject; AName: string;
  var AValue: string);
begin
  Writeln(Self.ClassName + 'LoadProperty: ' + Sender.ClassName + '.' + AName);
end;

procedure TCustomContainer.SaveProperty(Sender: TObject; AName, AValue: string);
begin
  Writeln(Self.ClassName + ' SaveProperty: ' + Sender.ClassName + '.' + AName);
end;

{ TIniContainer }

constructor TIniContainer.Create(AContainer: string);
begin
  inherited;
  FIniFile := TIniFile.Create(AContainer);
end;

destructor TIniContainer.Destroy;
begin
  FIniFile.UpdateFile;
  FIniFile.Free;
  inherited;
end;

procedure TIniContainer.LoadProperty(Sender: TObject; AName: string;
  var AValue: string);
begin
  inherited;
  FIniFile.ReadString(sender.ClassName, AName, AValue);
end;

procedure TIniContainer.SaveProperty(Sender: TObject; AName, AValue: string);
begin
  inherited;
  FIniFile.WriteString(sender.ClassName, AName, AValue);
end;

{ TTxtContainer }

constructor TTxtContainer.Create(AContainer: string);
begin
  inherited;
  FContainer := AContainer;
  FStringList := TStringList.Create;
  if FileExists(AContainer) then
    FStringList.LoadFromFile(AContainer);
end;

destructor TTxtContainer.Destroy;
begin
  FStringList.SaveToFile(FContainer);
  FStringList.Free;
  inherited;
end;

procedure TTxtContainer.LoadProperty(Sender: TObject; AName: string;
  var AValue: string);
begin
  inherited;
  AValue := FStringList.Values[Sender.ClassName+'.'+AName];
end;

procedure TTxtContainer.SaveProperty(Sender: TObject; AName, AValue: string);
begin
  inherited;
  FStringList.Values[Sender.ClassName+'.'+AName] := AValue;
end;

{ TRegContainer }

constructor TRegContainer.Create(AContainer: string);
begin
  inherited;
  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_LOCAL_MACHINE;
  FRegistry.OpenKey(AContainer, True);
end;

destructor TRegContainer.Destroy;
begin
  FRegistry.CloseKey;
  FRegistry.Destroy;
  inherited;
end;

procedure TRegContainer.LoadProperty(Sender: TObject; AName: string;
  var AValue: string);
begin
  inherited;
  AValue := FRegistry.ReadString(AName);
end;

procedure TRegContainer.SaveProperty(Sender: TObject; AName, AValue: string);
begin
  inherited;
  FRegistry.WriteString(AName, AValue);
end;



var a, b: TParent;
  containers: TObjectList;
  i: integer;
begin
  containers := TObjectList.Create(True);
  a := TChild1.Create;
  b := TChild2.Create;
  try
    containers.Add(TIniContainer.Create('file.ini'));
    containers.Add(TTxtContainer.Create('file.txt'));
    containers.Add(TRegContainer.Create('/mycomponent'));

    for i := 0 to containers.Count - 1 do
    begin
      a.container := TCustomContainer(containers[i]);
      a.save();
      a.clear();
      a.load();

      b.container := a.container;
      b.save();
      b.clear();
      b.load();
    end;

  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  containers.Free;  
  a.Free;
  b.Free;

  writeln ('Press ENTER to exit');
  readln;
end.
