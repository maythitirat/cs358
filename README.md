# cs358
#Name : Thitirat Thongthaew
#ID : 5709612120
File TrainID3.R คือไฟล์ใน lab
ตัวอย่างการเรียกใช้function InformationGain : 
  InformationGain(table(mushroom[,c('odor', 'class')]))
  InformationGain(table(mushroom[,c('cap.color', 'class')]))

ตัวอย่างการเรียกใช้function Attribute_Selection : 
  print(Attribute_Selection(mushroom))
