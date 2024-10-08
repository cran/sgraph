
customDrawLabel = function(
  context, data, settings, singleLine, drawFrame
) {
  if (!data.label) return;
  if (singleLine === undefined) {
      singleLine = true;
  }
  if (drawFrame === undefined) {
      drawFrame = true;
  }

  const size = settings.labelSize,
    font = settings.labelFont,
    weight = settings.labelWeight,
    color = "#000";
    //color = settings.labelColor.attribute
    //  ? data[settings.labelColor.attribute] || settings.labelColor.color || "#000"
    //  : settings.labelColor.color;

    const PADDING = 3;

  context.fillStyle = color;
  context.font = `${weight} ${size}px ${font}`;

  var lines = data.label.split("\n");
  var lineHeight = size;
  var y = data.y + size / 3;
  if (singleLine) {
    if (drawFrame) {
        customDrawHover(context, data, settings, true);
    }
    context.fillText(lines[0], data.x + data.size + 3, y);
  } else {
    for (var i = 0; i < lines.length; i++) {
      context.fillText(lines[i], data.x + data.size + 3, y);
      y += lineHeight + PADDING;
    }
  }
}

customDrawHover = function (
  context, data, settings, singleLine
  ) {
    const size = settings.labelSize,
      font = settings.labelFont,
      weight = "bold";

  if (singleLine === undefined) {
      singleLine = false;
  }
  
    context.font = `${weight} ${size}px ${font}`;
  
    context.fillStyle = "white";
    context.shadowOffsetX = 0;
    context.shadowOffsetY = 0;
    context.shadowBlur = 10;
    context.shadowColor = "black";


  
    const PADDING = 3;
  
    if (typeof data.label === "string") {
      var textWidth = context.measureText(data.label).width,
        boxWidth = Math.round(textWidth + 5),
        boxHeight = Math.round(size + 2 * PADDING),
        radius = Math.max(data.size, size / 2) + PADDING;
  
      const angleRadian = Math.asin(boxHeight / 2 / radius);
      const xDeltaCoord = Math.sqrt(
        Math.abs(Math.pow(radius, 2) - Math.pow(boxHeight / 2, 2))
      );

    var lines = data.label.split("\n");
        if (singleLine) {
            lines = lines.slice(0, 1);
        }
    //boxHeight = boxHeight * lines.length;
        
    //boxWidth = lines.forEach(function(line) { 
    //    this.measureText(line).width
    //})
  boxWidth = context.measureText(lines[0]).width
  for (var i = 1; i < lines.length; i++) {
      if (context.measureText(lines[i]).width > boxWidth) {
        boxWidth = context.measureText(lines[i]).width
      }
  }
        boxWidth = Math.round(boxWidth + 5);

       //if (singleLine) xDeltaCoord = 0;
  
      context.beginPath();
      context.moveTo(data.x + xDeltaCoord, data.y + boxHeight / 2 + Math.round(size + PADDING) * (lines.length - 1));
      context.lineTo(data.x + radius + boxWidth, data.y + boxHeight / 2 + Math.round(size + PADDING) * (lines.length - 1));
      context.lineTo(data.x + radius + boxWidth, data.y - boxHeight / 2);
      context.lineTo(data.x + xDeltaCoord, data.y - boxHeight / 2);
        if (!singleLine) {
      context.arc(data.x, data.y, radius, angleRadian, -angleRadian);
        }
      context.closePath();
      context.fill();
    } else {
      context.beginPath();
      context.arc(data.x, data.y, data.size + PADDING, 0, Math.PI * 2);
      context.closePath();
      context.fill();
    }
  
    context.shadowOffsetX = 0;
    context.shadowOffsetY = 0;
    context.shadowBlur = 0;
  
    // And finally we draw the label
    customDrawLabel(context, data, settings, singleLine, false);
  }


HTMLWidgets.widget({

  name: "sgraph",
  type: "output",

  factory: function(el, width, height){

    var graph = new graphology.Graph();
    var s = new Sigma(graph, document.getElementById(el.id));

    return {
      renderValue: function(x){
        if (HTMLWidgets.shinyMode) { // If in Shiny app
          // Remove previous occurences of plots in the <div>
          sigmaID = document.getElementById(el.id)
          while (sigmaID.firstChild) {
            //The list is LIVE so it will re-index each call
            sigmaID.removeChild(sigmaID.firstChild);
          }
          graph = new graphology.Graph();
          //graph.import(x.data);
          s = new Sigma(graph, sigmaID, {
         //     labelColor: { color: x.options.labelColor }, 
              hoverRenderer: customDrawHover,
              labelRenderer: customDrawLabel,
    //          edgeReducer: (edge, data) => {
  //return {...data, size:5 }
//},
              zIndex: true
            });
          s.refresh();
        }

        //s.setSetting('labelColor', x.options.labelColor);
        s.setSetting('labelGridCellSize', x.options.label_grid_cell_size);
        s.setSetting('minEdgeSize', x.options.min_edge_size);
        s.setSetting('maxEdgeSize', x.options.max_edge_size);
        s.setSetting('minNodeSize', x.options.min_node_size);
        s.setSetting('maxNodeSize', x.options.max_node_size);
        s.setSetting('doubleClickEnabled', x.options.doubleClickZoom);
        s.setSetting('mouseWheelEnabled', x.options.mouseWheelZoom);
        s.graph.import(x.data);


        //s.on("enterNode", function(d){
        //  s.setNodeattribute(d.data.node, 'label', s.graph.getNodeAttribute(n, 'word'))
        //  s.setNodeattribute(d.data.node, 'word', s.graph.getNodeAttribute(n, 'id') + s.graph.getNodeAttribute(n, 'desc'))
        //})

        //s.on("leaveNode", function(d){
        //  s.setNodeattribute(d.data.node, 'word', 'label')
        //})

        if (x.options.neighborEvent != 'None') {
          s.graph.nodes().forEach(function(n) {
            s.graph.setNodeAttribute(n, 'original_color', s.graph.getNodeAttribute(n, 'color'));
            s.graph.setNodeAttribute(n, 'original_hidden', s.graph.getNodeAttribute(n, 'hidden'));
          });
          s.graph.edges().forEach(function(e) {
            s.graph.setEdgeAttribute(e, 'original_color', s.graph.getEdgeAttribute(e, 'color'));
            //e.type = x.options.edgeArrows;
          });
          s.on(x.options.neighborStart, function(e) {
            var nodeId = e.node,
                toKeep = s.graph.neighbors(nodeId);
            toKeep.push(e.node);
            s.graph.nodes().forEach(function(n) {
              if (toKeep.includes(n)) {
                s.graph.setNodeAttribute(n, 'color', s.graph.getNodeAttribute(n, 'original_color'));
                s.graph.setNodeAttribute(n, 'hidden', false);
              } else {
                s.graph.setNodeAttribute(n, 'color', '#eee');
              }
            });
            s.graph.edges().forEach(function(e) {
              if (toKeep.includes(s.graph.source(e)) && toKeep.includes(s.graph.target(e)))
                s.graph.setEdgeAttribute(e, 'color', s.graph.getEdgeAttribute(e, 'original_color'));
              else
                s.graph.setEdgeAttribute(e, 'color', '#eee');
            });
            s.refresh();
          });
          s.on(x.options.neighborEnd, function(e) {
            s.graph.nodes().forEach(function(n) {
                s.graph.setNodeAttribute(n, 'color', s.graph.getNodeAttribute(n, 'original_color'));
                s.graph.setNodeAttribute(n, 'hidden', s.graph.getNodeAttribute(n, 'original_hidden'));
            });
            s.graph.edges().forEach(function(e) {
                s.graph.setEdgeAttribute(e, 'color', s.graph.getEdgeAttribute(e, 'original_color'));
            });
            s.refresh();
          });

          if (HTMLWidgets.shinyMode) {
            if (x.options.sigmaEvents) {
              if (x.options.sigmaEvents == 'clickNode') {
                s.on("clickNode", function(d) {
                  Shiny.onInputChange('current_node_id', d.node)
                })
              }
              if (x.options.sigmaEvents == 'hoverNode') {
                s.on("overNode", function(d) {
                  Shiny.onInputChange('current_node_id', d.node)
                })
              }
            }
          }
        }


        s.refresh();
      },
      resize: function(width, height){
        for(var name in s.renderers)
          s.renderers[name].resize(width, height);
      },
      s: s
    };
  }
})
