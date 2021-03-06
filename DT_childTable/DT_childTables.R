library(DT)

## data
dat <- data.frame(
  Sr = c(1.5, 2.3),
  Description = c("A - B", "X - Y")
)
## details of row 1
subdat1 <- data.frame(
  Chromosome = "chr18", 
  SNP = "rs2",
  stringsAsFactors = FALSE
)
## details of row 2
subdat2 <- data.frame(
  Chromosome = c("chr19","chr20"), 
  SNP = c("rs3","rs4"), 
  stringsAsFactors = FALSE
)

## merge the row details
subdats <- lapply(list(subdat1, subdat2), purrr::transpose)
## dataframe for the datatable
Dat <- cbind(" " = "&oplus;", dat, details = I(subdats))

## the callback
callback = JS(
  "table.column(1).nodes().to$().css({cursor: 'pointer'});",
  "// Format the nested table into another table",
  "var childId = function(d){",
  "  var tail = d.slice(2, d.length - 1);",
  "  return 'child_' + tail.join('_').replace(/[\\s|\\.]/g, '_');",
  "};",
  "var format = function (d) {",
  "  if (d != null) {",
  "    var id = childId(d);",
  "    var html = ", 
  "          '<table class=\"display compact\" id=\"' + id + '\"><thead><tr>';",
  "    for (var key in d[d.length-1][0]) {",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",
  "var rowCallback = function(row, dat, displayNum, index){",
  "  if($(row).hasClass('odd')){",
  "    for(var j=0; j<dat.length; j++){",
  "      $('td:eq('+j+')', row).css('background-color', 'papayawhip');",
  "    }",
  "  } else {",
  "    for(var j=0; j<dat.length; j++){",
  "      $('td:eq('+j+')', row).css('background-color', 'lemonchiffon');",
  "    }",
  "  }",
  "};",
  "var headerCallback = function(thead, data, start, end, display){",
  "  $('th', thead).css({",
  "    'border-top': '3px solid indigo',", 
  "    'color': 'indigo',",
  "    'background-color': '#fadadd'",
  "  });",
  "};",
  "var format_datatable = function (d) {",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for (var i = 0; i < d[n].length; i++) {",
  "    var datarow = $.map(d[n][i], function (value, index) {",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId(d);",
  "  var subtable = $(id).DataTable({",
  "                     'data': dataset,",
  "                     'autoWidth': true,",
  "                     'deferRender': true,",
  "                     'info': false,",
  "                     'lengthChange': false,",
  "                     'ordering': d[n].length > 1,",
  "                     'order': [],",
  "                     'paging': false,",
  "                     'scrollX': false,",
  "                     'scrollY': false,",
  "                     'searching': false,",
  "                     'sortClasses': false,",
  "                     'rowCallback': rowCallback,",
  "                     'headerCallback': headerCallback,",
  "                     'columnDefs': [{targets: '_all', className: 'dt-center'}]",
  "                   });",
  "};",
  "table.on('click', 'td.details-control', function () {",
  "  var td = $(this),",
  "      row = table.row(td.closest('tr'));",
  "  if (row.child.isShown()) {",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    row.child(format(row.data())).show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data());",
  "  }",
  "});")

## datatable
datatable(Dat, callback = callback, escape = -2,
          options = list(
            columnDefs = list(
              list(visible = FALSE, targets = ncol(Dat)),
              list(orderable = FALSE, className = 'details-control', targets = 1),
              list(className = "dt-center", targets = "_all")
            )
          ))
