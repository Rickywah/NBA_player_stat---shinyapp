calc_EFF = function(data){
  data |>
    mutate(EFF = (PTS + TRB + AST + STL + BLK - MFT - MFG - TOV) / G) |>
    mutate(EFF = round(EFF, digits = 3))
}
