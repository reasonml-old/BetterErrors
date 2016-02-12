module TopNotchModule = struct
  type jeSuisString = string
  type woShiRecordIHaveAReallyLongNameThatllMakeYouLineBreak = {nihao: int}
end

let asd: TopNotchModule.jeSuisString = ({nihao = 1}: TopNotchModule.woShiRecordIHaveAReallyLongNameThatllMakeYouLineBreak)
