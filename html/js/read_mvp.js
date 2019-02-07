d3.csv("2018.csv").then(function(data) {
  for (i=0; i< data.length; i++){
    create_table(data[i].Player,data[i].Pred);
  }
});

function create_table(name, pred) {
  var table = document.getElementById("mvp_table");
  var row = table.insertRow(0);
  var cell1 = row.insertCell(0);
  var cell2 = row.insertCell(1);
  cell1.innerHTML = name;
  cell2.innerHTML = pred;
}