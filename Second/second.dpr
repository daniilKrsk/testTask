﻿program second;

{
Нужно реализовать класс или группу классов, которые реализуют хранение
объектов в виде дерева. Этот класс (группа классов) должны предоставлять
следующие возможности:
 создать дерево,
 добавить заданному узлу дочерний узел,
 получить доступ к дочерним узлам заданного узла,
 получить доступ к родительскому узлу заданного узла,
 записать в заданный узел объект,
 получить доступ к объекту, хранимому в заданном узле.
Тип хранимого объекта TObject, не надо его реализовывать.
Никаких ограничений на количество уровней и дочерних узлов не должно быть.
Вы не должны использовать готовые классы, реализующие деревья, но можете
пользоваться любыми другими коллекциями. Результатом является pas файл с
реализацией классов.
}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  TreeObject in 'TreeObject.pas',
  TreeContainer in 'TreeContainer.pas';

var
  tc: TTreeContainer;
  Node, n1, n2: TTreeObject;
begin
  tc := TTreeContainer.Create;
  try
    Node := tc.Root;
    Node.AddSample(1, 3); // 10 уровней по 2 узла

  { n1 := Node.CreateChild('1');

   n1.CreateChild('2');
   n1.CreateChild('3');
   {
    n2.Info := '-1';
    n1.CreateChild.Info := '-2';

    n2.CreateChild.Info := 'n2-1';
    n2.CreateChild.Info := 'n2-1';
   }
    tc.Root.PrintTree();

   writeln('');
   writeln ('Press ENTER to exit');
   readln;
   
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.