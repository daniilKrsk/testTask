unit TreeContainer;

interface

uses TreeObject;

type
  TTreeContainer = class
  private
    FRoot: TTreeObject;
  public
    property Root: TTreeObject read FRoot;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TTreeContainer }

constructor TTreeContainer.Create;
begin
  FRoot := TTreeObject.Create;
  FRoot.Info := 'Root';
end;

destructor TTreeContainer.Destroy;
begin
  FRoot.Free;
  inherited;
end;

end.
