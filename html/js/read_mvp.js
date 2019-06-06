d3.csv("2019Pred.csv").then(function(data) {
  for (i=0; i< data.length; i++){
    create_table(data[i].Player,data[i].Pct);
  }
});

function create_table(name, pct) {
  var table = document.getElementById("mvp_table");
  var row = table.insertRow(table.rows.length);
  var cell1 = row.insertCell(0);
  var cell2 = row.insertCell(1);
  cell1.innerHTML = name;
  if (pct < 1){
    cell2.innerHTML = "⠀<1%";
  } else {
    cell2.innerHTML = "⠀"+Math.trunc(pct)+"%";
  }
}