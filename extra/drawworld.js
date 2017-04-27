var urls = { // map from simulation names to urls
  solar:  "/solar",
  world4: "/world4",
  world2: "/world2",
  world2neg: "/world2neg"
};
// Important: changes to global variables
// may only happen inside JSON handlers
var interval = null;
var curSimName = null;
var curWorld = null; // constantly changing world
var skip = 0; // skip n frames if congested
var dimX = null;
var dimY = null;

$(document).ready(function() {
    $("#reset").click(function() {reset(curSimName);});
    $("select").change(function() {
        var str = $("select option:selected").val();
        reset(str);
    });
    $("select option[value='solar']").attr('selected', 'selected');
    reset("solar");
    $.get("/display-params", function(data) {
      var vals = data.split(' ');
      interval = 1000.0 / parseInt(vals[0]);
      dimX = vals[1];
      dimY = vals[2];
      setInterval(advance, interval);
    });
});

function reset(simName) {
    if (!urls[simName])
        alert("Error: invalid simulation: " + simName);
    $.getJSON(urls[simName], function(newWorld){
        newWorld.seqNum = curWorld? curWorld.seqNum + 1: 0;
        curSimName = simName;
        curWorld = newWorld;
    });
}

// Called in a loop
function advance() {
    if (skip == 0) {
        drawWorld();
        refreshWorld();
    } else
        skip -= 1;
}

function refreshWorld(simName) {
    // Get new world from server
    $.ajax(
    {
       "data"    : JSON.stringify(curWorld),
       "type"    : "POST",
       "url"     : "/advance",
       "success" : updateWorld
    });
}

// Handler called with new world
function updateWorld(newWorld)
{
   if(!curWorld) alert("null world!");
   var lag = curWorld.seqNum - newWorld.seqNum;
   if (lag == 0) {
       curWorld = newWorld;
       curWorld.seqNum += 1;
   } else if (lag > 0)
       skip = lag;
   else
       alert("Time travel discovered!")
}

function canvas_arrow(context, fromx, fromy, tox, toy){
  var headlen = 10; // length of head in pixels
  var dx = tox-fromx;
  var dy = toy-fromy;
  var angle = Math.atan2(dy,dx);
  context.moveTo(fromx, fromy);
  context.lineTo(tox, toy);
  context.lineTo(tox-headlen*Math.cos(angle-Math.PI/6),
                 toy-headlen*Math.sin(angle-Math.PI/6));
  context.moveTo(tox, toy);
  context.lineTo(tox-headlen*Math.cos(angle+Math.PI/6),
                 toy-headlen*Math.sin(angle+Math.PI/6));
}

function drawWorld() {
  if (!curWorld) return true; // might happen

  var canvas = document.getElementById('sky');
  var ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, dimX, dimY);
  ctx.fillStyle = "white";
  ctx.strokeStyle = "white";

  // Draw particles
  var partsInView = 0;
  for (var j = 0; j < curWorld.parts.length; j++) {
    var part = curWorld.parts[j],
        size = Math.log(part.pmass/curWorld.pixInKg) / Math.LN10;
    if (size < 2) size = 2;
    var x = dimX/2 + curWorld.pixInM * part.ppos.posx,
        y = dimY/2 + curWorld.pixInM * part.ppos.posy,
        color = Math.round(part.pchar / curWorld.maxCharge * 255);
    if ( x > -10 && x < dimX + 10 && y > -10 && y < dimY + 10) {
      partsInView += 1;
      if (color >= 0) {
        color = 255 - color;
        ctx.fillStyle = 'rgb(255, ' + color + ', ' + color + ')';
      } else {
        color = 255 - Math.abs(color);
        ctx.fillStyle = 'rgb(' + color + ', ' + color + ', 255)';
      }
      ctx.beginPath();
      ctx.arc(x, y, size/2, 0, Math.PI * 2, true);
      ctx.fill();
    }
  }
  // Draw samples
  for (var j = 0; j < curWorld.samples.length; j++) {
    var sample = curWorld.samples[j];
    var x = dimX/2 + sample.spos.posx * curWorld.pixInM,
        y = dimY/2 + sample.spos.posy * curWorld.pixInM,
        fx = sample.sfor.fx * curWorld.pixInN,
        fy = sample.sfor.fy * curWorld.pixInN;
    ctx.beginPath();
    canvas_arrow(ctx, x, y, x+fx, y+fy);
    ctx.stroke();
    partsInView += 1;
  }
  return partsInView != 0;
}