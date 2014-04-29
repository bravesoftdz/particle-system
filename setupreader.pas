unit setupreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Vectors,partSystem;

type

  { setupData }

  SetupData=
    class
      Cathegories:TList;
      procedure ReadFromStream(stream:TStream);
    end;

  { SetupCath }
  SetupElement=class;

  SetupCath=
    class
      Name:string;
      elements:TList;
      multiplier:SetupElement;
      constructor create;
      function PickRandomElement:SetupElement;
    end;

  { SetupElement }

  SetupElement=
    class
      Name:string;
      Checked:boolean;
      Color:Vector3D;
      Amount:double;
      AmountVar:double;
      Mass,charge:double;
      MassVar,ChargeVar:double;
      Pos:Vector3D;
      PosVar:double;
      Size:double;SizeVar:double;
      Speed:Vector3D;
      SpeedVar:double;
      Period:double;
      Dielectric,Ferro:double;
      DielectricVar,FerroVar:double;
      Spin:Vector3D;
      SpinVar:double;
      function GenerateParticle(Multiplier:SetupElement):Particle;
      constructor create;
    end;

function ReadFromStream(stream:TStream):setupData;

implementation

var varNameList:TstringList;

function ReadFromStream(stream:TStream):setupData;
begin
  result:= setupData.create;
  result.cathegories:= TList.create;
  result.ReadFromStream(stream);
end;

{ SetupElement }

function SetupElement.GenerateParticle(Multiplier: SetupElement): Particle;
  function PM(range:double):double;
  begin
    if range=0
      then result:= 0
      else result:= range-random*2*range;
  end;
  procedure Multi(var val:double; const mul:double);
  begin
    if mul<>0
      then val:= val*mul;
  end;
  procedure Multi3D(var val:Vector3D;const mul:Vector3D);
  begin
    if mul[0]<>0
      then Factor(val,val,mul[0]);
  end;

var p:Particle; i:integer; alpha,r,per:double;
begin
  p:= Particle.create;
  p.id:= name;
  p.color:= (round(color[2])*256+round(color[1]))*256+round(color[0]);
  if multiplier=nil then
  begin
    if period=0 then
    begin
      for i:= 0 to 2 do
      begin
        p.pos[i]:= pos[i] + PM(PosVar);
        p.speed[i]:= speed[i]+ PM(speedVar);
      end;
    end
    else
    begin
      r:= pos[0]+PM(posVar);
      alpha:= pi*2*random;
      p.pos[0]:= r*sin(alpha);
      p.pos[1]:= r*cos(alpha);
      p.pos[2]:= 0;
      r:= r/period;
      p.speed[0]:= r*cos(alpha);
      p.speed[1]:= -r*sin(alpha);
      p.speed[2]:= 0;
    end;
    p.restmass:= mass+PM(massVar);
    p.electric_charge:= charge+PM(chargeVar);
    p.radius:= size+PM(sizeVar);
    for i:= 0 to 2 do
    begin
      p.rest_magnetic_impulse[i]:= spin[i]+PM(spinVar);
    end;
    p.ferromagnetic:= ferro+ PM(ferroVar);
    p.dielectric:= Dielectric+ PM(dielectricVar);
  end
  else
  begin
      if period=0 then
      begin
        for i:= 0 to 2 do
        begin
          p.pos[i]:= pos[i] + PM(PosVar);
          p.speed[i]:= speed[i]+ PM(speedVar);
          Multi3D(pos,multiplier.pos);
          Multi3D(speed,multiplier.speed);
        end;
      end
      else
      begin
        r:= pos[0]+PM(posVar);
        per:= period;
        Multi(r,multiplier.pos[0]);
        Multi(per,multiplier.Period);
        alpha:= pi*2*random;
        p.pos[0]:= r*sin(alpha);
        p.pos[1]:= r*cos(alpha);
        p.pos[2]:= 0;
        p.speed[0]:= PI*2*r*cos(alpha)/per;
        p.speed[1]:= -PI*2*r*sin(alpha)/per;
        p.speed[2]:= 0;
      end;
      p.restmass:= mass+PM(massVar);
      p.electric_charge:= charge+PM(chargeVar);
      p.radius:= size+PM(sizeVar);
      Multi(p.restmass,multiplier.mass);
      Multi(p.electric_charge,multiplier.charge);
      Multi(p.radius,multiplier.size);
      for i:= 0 to 2 do
      begin
        p.rest_magnetic_impulse[i]:= spin[i]+PM(spinVar);
      end;
      p.ferromagnetic:= ferro+ PM(ferroVar);
      p.dielectric:= Dielectric+ PM(dielectricVar);
      Multi3D(p.rest_magnetic_impulse,multiplier.Spin);
      Multi(p.ferromagnetic,multiplier.Ferro);
      Multi(p.dielectric,multiplier.Dielectric);
  end;
  result:= p;
end;

constructor SetupElement.create;
begin
  amount:= 1;
end;

{ SetupCath }

constructor SetupCath.create;
begin
  self.elements:= TList.create;
end;

function SetupCath.PickRandomElement: SetupElement;
var i,checked,p:integer; el:SetupElement; pick,sum,totalAmount:double;
begin
  result:= nil;
  totalAmount:= 0;
  checked:= 0;
  for i:= 0 to self.elements.count-1 do
    begin
      el:= SetupElement(self.elements[i]);
      totalAmount:= totalAmount+el.amount;
      if el.checked then checked:= checked+1;
    end;
  if totalAmount>=1 then
  begin
    pick:= totalAmount*random;
    for i:= 0 to self.elements.count-1 do
      begin
        el:= SetupElement(elements[i]);
        sum:= sum+el.amount;
        if sum>= pick then
        begin
          result:= el;
          EXIT;
        end;
      end;
  end;
  if self.elements.count>0 then
  begin
    if checked>0 then
    begin
      checked:= 0;
      p:= random(checked);
      for i:= 0 to self.elements.count-1 do
        begin
          el:= SetupElement(elements[i]);
          if el.checked
            then checked:= checked+1;
          if p<checked
            then result:= el;
        end;
    end
    else
    begin
      p:= random(elements.count);
      result:= SetupElement(elements[p]);
    end;
  end;
end;


{ setupData }

procedure SetupData.ReadFromStream(stream: TStream);
  var currentCath:SetupCath;
  var currentElement:SetupElement;
  procedure ReadLine(const line:string);
  var k:integer;
    function First:string;
    begin
      result:= '';
      if (k<=length(line))
        then result:= line[k];
    end;
    function ScanChars(const chars:string;rangeFrom,RangeTo:string;caps:boolean):string;
    var found:boolean; f:string;
    var start:integer;
    begin
      start:= k;
      repeat
        found:=false;
        f:= first;
        if f<>'' then
        begin
          if caps then f:= Uppercase(f);
          if (rangeFrom<>'')and(rangeTo<>'') then
          begin
            if (f>=rangeFrom)and(f<=RangeTo)
              then found:= true;
          end;
          if not found then
            if chars<>'' then
              if pos(f,chars)<>0
                then found:= true;
          if found
            then k:= k+1;
        end;
      until not found;
      result:= copy(line,start,k-start);
    end;
    function ScanTill(const finish:string;atStart:boolean;skipEnd:integer):string;
    var j,len:integer; cp:string;
    begin
      result:= '';
      len:= length(finish);
      j:= 0;
      while ((k+j+len-1)<=length(line)) do
      begin
        cp:= copy(line,k+j,len);
        if cp= finish then
        begin
          result:= copy(line,k,j+len-skipEnd);
          k:= k+j+len;
          EXIT;
        end;
        if atStart
          then EXIT;
        j:= j+1;
      end;
    end;
    function Scan(const item:string):boolean;
    var cp:string;
    begin
      result:= false;
      cp:= copy(line,k,length(item));
      if cp=item then
      begin
        result:= true;
        k:= k+length(item);
      end;
    end;

    procedure ScanWS; //WhiteSpace;
    begin
      while Scan(' ')or scan(#8) do
      begin
      end;
    end;
    function ReadCath:boolean;
    var cathName:string;
    begin
      result:= false;
      if Scan('[') then
      begin
        cathName:= ScanTill(']',false,1);
        result:= true;

        CurrentCath:= SetupCath.create;
        self.Cathegories.add(currentCath);
        CurrentCath.Name:= cathName;
      end;
    end;
    function ReadElement:boolean;
    var elName:string;
    begin
      result:= false;
      if Scan('<') then
      begin
        elName:= ScanTill('>',false,1);
        result:= true;

        CurrentElement:= SetupElement.create;
        CurrentElement.name:= elName;
        if copy(elName,1,1)='*'
          then CurrentCath.multiplier:= CurrentElement
          else CurrentCath.elements.add(CurrentElement);
      end;
    end;
    procedure ReadItem;
      function readValue:double;
      var value:string; d:double; err:integer;
      begin
        result:= 0;
        ScanWS;
        value:= ScanChars('.E-','0','9',false);
        if value<>'' then
        begin
          val(value,d,err);
          if err<>0
            then result:= 0
            else result:= d;
          ScanWS;
        end;
      end;
    var varname:string;
        valueX,valueY,valueZ,valueVar:double; vec:Vector3D;
    begin
      valueX:= 0; valueY:= 0; valueZ:= 0; valueVar:= 0;
      varname:= ScanChars('','A','Z',true);
      ScanWS;
      if Scan('=') then
      begin
        ScanWS;
        valueX:= ReadValue;
        if Scan(',')
          then valueY:= ReadValue;
        if Scan(',')
          then valueZ:= ReadValue;
        if Scan('+-')
          then valueVar:= ReadValue;
        SetVector(vec,valueX,valueY,valueZ);
      end;
      with CurrentElement do
      case varname of
        'Default','Checked':checked:= true;
        'Color':begin Color:= Vec; end;
        'Amount':begin Amount:= valueX; AmountVar:= valueVar; end;
        'Mass': begin Mass:= valueX; MassVar:= valueVar; end;
        'Charge': begin Charge:= valueX; ChargeVar:= valueVar; end;
        'Pos','Position': begin pos:= vec; posVar:= valueVar; end;
        'Size':begin size:= valueX; sizeVar:= valueVar; end;
        'Speed': begin speed:= vec; speedVar:= valueVar; end;
        'Period': begin period:= valueX; end;
        'Dielectric': begin Dielectric:= valueX; DielectricVar:= valueVar; end;
        'Ferro': begin Ferro:= valueX; FerroVar:= valueVar; end;
        'Spin': begin Spin:= vec; SpinVar:= valueVar; end;
      end;
    end;
    procedure ReadItems;
    begin
      repeat
        ScanWS;
        ReadItem;
        ScanWS;
      until (not Scan(';'));
    end;

  begin
    k:= 1;
      ScanWS;
      if ReadCath then
      begin
      end
      else
      if CurrentCath<>nil then
      begin
        if ReadElement then
        else if CurrentElement<>nil then
        begin
          ReadItems;
        end;
      end;
  end;

var list:TStringList;
var i:integer;
begin
  CurrentCath:= nil;
  CurrentElement:= nil;
  list:= TStringList.create;
  list.LoadFromStream(stream);
  for i:= 0 to list.count-1 do
  begin
    ReadLine(list[i]);
  end;
end;

end.

