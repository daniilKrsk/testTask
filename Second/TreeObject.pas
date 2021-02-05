unit TreeObject;

interface

uses SysUtils;

type
  TTreeObject = class
  private
    FParent: TTreeObject;
    FFirstChild: TTreeObject;
    FNextSibling: TTreeObject;
    FData: TObject;
    FPrevSibling: TTreeObject;
    procedure SetParent(const Value: TTreeObject);
    procedure SetFirstChild(const Value: TTreeObject);
    procedure SetNextSibling(const Value: TTreeObject);
    procedure SetData(const Value: TObject);
    procedure SetPrevSibling(const Value: TTreeObject);
    function GetLastChild: TTreeObject;
  public
    Info: string;
    procedure Extract;
    constructor Create; 
    function CreateChild(AInfo: string = 'n'): TTreeObject;
    procedure AddSample(ALevel, ACnt: integer);
    procedure PrintTree(ALevel: integer = 0);
    property Parent: TTreeObject read FParent write SetParent;
    property NextSibling: TTreeObject read FNextSibling write SetNextSibling;
    property PrevSibling: TTreeObject read FPrevSibling write SetPrevSibling;
    property LastChild: TTreeObject read GetLastChild;
    property FirstChild: TTreeObject read FFirstChild write SetFirstChild;
    property Data: TObject read FData write SetData;
    destructor Destroy; override;
  end;

implementation

{ TTreeObject }

procedure TTreeObject.AddSample(ALevel, ACnt: integer);
var
  i: Integer;
begin
  if(ALevel > 0) then
    for i := 1 to ACnt do
      CreateChild(Format('lvl:%d, n:%d', [ALevel, i]));//.AddSample(ALevel - 1, ACnt);
end;

constructor TTreeObject.Create;
begin
  NextSibling := nil;
  PrevSibling := nil;
  FirstChild := nil;
  FParent := nil;
  
  Info := 'N';
end;

function TTreeObject.CreateChild(AInfo: string): TTreeObject;
begin
  Result := TTreeObject.Create;
  Result.Parent := self;
  Result.Info := AInfo;

  if Assigned(FirstChild)then
  begin
    Result.PrevSibling := LastChild;
    Result.PrevSibling.NextSibling := Result;
  end
  else
    FirstChild := Result
end;

destructor TTreeObject.Destroy;
var
  Node, DelNode: TTreeObject;
begin
  Extract;

  Node := NextSibling;
  while (Node <> nil) do
  begin
    DelNode := Node; // curnode
    Node := Node.NextSibling;
    DelNode.Free;
  end;
  
  inherited;
end;

procedure TTreeObject.Extract;
begin
  if Assigned(Parent) and (Parent.FirstChild = self) then
    Parent.FirstChild := NextSibling; 
  if Assigned(NextSibling) then
    NextSibling.PrevSibling := PrevSibling;
  if Assigned(PrevSibling) then
    PrevSibling.NextSibling := NextSibling;
end;

function TTreeObject.GetLastChild: TTreeObject;
begin
  Result := FirstChild;

  if(Result = nil)then
    Exit;

  while (Result.NextSibling <> nil) do
    Result := Result.NextSibling;
end;

procedure TTreeObject.PrintTree(ALevel: integer);
var
  str: string;
  ANode: TTreeObject;
begin
  SetLength(str, ALevel * 4);
  FillChar(str[1], ALevel * 4, '-');

  WriteLn(str + '>'+ Info);

  if Assigned(FirstChild) then
    FirstChild.PrintTree(ALevel + 1);
  
  ANode := self.NextSibling;
  while ANode <> nil do
  begin
    ANode.PrintTree(ALevel);
    ANode := ANode.NextSibling;
  end;
end;

procedure TTreeObject.SetData(const Value: TObject);
begin
  FData := Value;
end;

procedure TTreeObject.SetFirstChild(const Value: TTreeObject);
begin
  FFirstChild := Value;
end;

procedure TTreeObject.SetNextSibling(const Value: TTreeObject);
begin
  FNextSibling := Value;
end;

procedure TTreeObject.SetParent(const Value: TTreeObject);
begin
  FParent := Value;
end;

procedure TTreeObject.SetPrevSibling(const Value: TTreeObject);
begin
  FPrevSibling := Value;
end;

end.
