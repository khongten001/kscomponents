unit untMenu;

interface

uses
  System.SysUtils, System.Classes, ksSlideMenu;

type
  TdmMenu = class(TDataModule)
    ksSlideMenu1: TksSlideMenu;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMenu: TdmMenu;

implementation

uses untForm1, untForm2, untForm3, ksTypes;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TdmMenu.DataModuleCreate(Sender: TObject);
begin
  dmMenu.ksSlideMenu1.AddMenuItem('','First Form', Form1, TksStandardIcon.Calendar);
  dmMenu.ksSlideMenu1.AddMenuItem('','Second Form', Form2, TksStandardIcon.BarChart);
  dmMenu.ksSlideMenu1.AddMenuItem('','Third Form', Form3, TksStandardIcon.Settings);
end;

end.
