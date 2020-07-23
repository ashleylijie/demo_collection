library(DT)

##~~ Multiple levels of nesting ~~##

## data
dat <- data.frame(
  Sr = c(1.5, 2.3),
  Description = c("A - B", "X - Y")
)
## details of row 1
subsubdat1 <- data.frame(
  Ref = c("UVW", "PQR"),
  Case = c(99, 999),
  stringsAsFactors = FALSE
)
subdat1 <- data.frame(
  Chromosome = "chr18", 
  SNP = "rs2",
  "_details" = I(list(purrr::transpose(subsubdat1))),
  stringsAsFactors = FALSE, 
  check.names = FALSE
)
subdat1 <- cbind(" " = "&oplus;", subdat1, stringsAsFactors = FALSE)
## details of row 2
subdat2 <- data.frame(
  Chromosome = c("chr19","chr20"), 
  SNP = c("rs3","rs4"), 
  stringsAsFactors = FALSE
)

## merge the row details
subdats <- lapply(list(subdat1, subdat2), purrr::transpose)
## dataframe for the datatable
Dat <- cbind(" " = "&oplus;", dat, "_details" = I(subdats))

## the callback
callback = JS(
  "table.column(1).nodes().to$().css({cursor: 'pointer'});",
  "",
  "// make the table header of the nested table",
  "var format = function(d, childId){",
  "  if(d != null){",
  "    var html = ", 
  "      '<table class=\"display compact hover\" id=\"' + childId + '\"><thead><tr>';",
  "    for (var key in d[d.length-1][0]) {",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",
  "",
  "// row callback to style the rows of the child tables",
  "var rowCallback = function(row, dat, displayNum, index){",
  "  if($(row).hasClass('odd')){",
  "    $(row).css('background-color', 'papayawhip');",
  "    $(row).hover(function(){",
  "      $(this).css('background-color', '#E6FF99');",
  "    }, function() {",
  "      $(this).css('background-color', 'papayawhip');",
  "    });",
  "  } else {",
  "    $(row).css('background-color', 'lemonchiffon');",
  "    $(row).hover(function(){",
  "      $(this).css('background-color', '#DDFF75');",
  "    }, function() {",
  "      $(this).css('background-color', 'lemonchiffon');",
  "    });",
  "  }",
  "};",
  "",
  "// header callback to style the header of the child tables",
  "var headerCallback = function(thead, data, start, end, display){",
  "  $('th', thead).css({",
  "    'border-top': '3px solid indigo',", 
  "    'color': 'indigo',",
  "    'background-color': '#fadadd'",
  "  });",
  "};",
  "",
  "// make the datatable",
  "var format_datatable = function(d, childId){",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for(var i = 0; i < d[n].length; i++){",
  "    var datarow = $.map(d[n][i], function (value, index) {",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId;",
  "  if (Object.keys(d[n][0]).indexOf('_details') === -1) {",
  "    var subtable = $(id).DataTable({",
  "                 'data': dataset,",
  "                 'autoWidth': true,",
  "                 'deferRender': true,",
  "                 'info': false,",
  "                 'lengthChange': false,",
  "                 'ordering': d[n].length > 1,",
  "                 'order': [],",
  "                 'paging': false,",
  "                 'scrollX': false,",
  "                 'scrollY': false,",
  "                 'searching': false,",
  "                 'sortClasses': false,",
  "                 'rowCallback': rowCallback,",
  "                 'headerCallback': headerCallback,",
  "                 'columnDefs': [{targets: '_all', className: 'dt-center'}]",
  "               });",
  "  } else {",
  "    var subtable = $(id).DataTable({",
  "            'data': dataset,",
  "            'autoWidth': true,",
  "            'deferRender': true,",
  "            'info': false,",
  "            'lengthChange': false,",
  "            'ordering': d[n].length > 1,",
  "            'order': [],",
  "            'paging': false,",
  "            'scrollX': false,",
  "            'scrollY': false,",
  "            'searching': false,",
  "            'sortClasses': false,",
  "            'rowCallback': rowCallback,",
  "            'headerCallback': headerCallback,",
  "            'columnDefs': [", 
  "              {targets: -1, visible: false},", 
  "              {targets: 0, orderable: false, className: 'details-control'},", 
  "              {targets: '_all', className: 'dt-center'}",
  "             ]",
  "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
  "  }",
  "};",
  "",
  "// display the child table on click",
  "table.on('click', 'td.details-control', function(){",
  "  var tbl = $(this).closest('table'),",
  "      tblId = tbl.attr('id'),",
  "      td = $(this),",
  "      row = $(tbl).DataTable().row(td.closest('tr')),",
  "      rowIdx = row.index();",
  "  if(row.child.isShown()){",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    var childId = tblId + '-child-' + rowIdx;",
  "    row.child(format(row.data(), childId)).show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data(), childId);",
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

# other example ####
library(data.table)

mtcars_dt <- data.table(mtcars)
setkey(mtcars_dt, mpg, cyl)

mpg_dt <- unique(mtcars_dt[, list(mpg, cyl)])
setkey(mpg_dt, mpg, cyl)

cyl_dt <- unique(mtcars_dt[, list(cyl)])
setkey(cyl_dt, cyl)

mtcars_dt <- 
  mtcars_dt[, list("_details" = list(purrr::transpose(.SD))), by = list(mpg,cyl)]
mtcars_dt[, ' ' := '&oplus;']

mpg_dt <- merge(mpg_dt, mtcars_dt, all.x = TRUE)
setkey(mpg_dt, cyl)
setcolorder(mpg_dt, c(length(mpg_dt), c(1:(length(mpg_dt) - 1))))

mpg_dt <- mpg_dt[,list("_details" = list(purrr::transpose(.SD))), by = cyl]
mpg_dt[, ' ' := '&oplus;']

cyl_dt <- merge(cyl_dt, mpg_dt, all.x = TRUE)
setcolorder(cyl_dt, c(length(cyl_dt), c(1:(length(cyl_dt) - 1))))

datatable(cyl_dt, callback = callback, escape = -2,
          options = list(
            columnDefs = list(
              list(visible = FALSE, targets = ncol(cyl_dt)),
              list(orderable = FALSE, className = 'details-control', targets = 1),
              list(className = "dt-center", targets = "_all")
            )
          ))

