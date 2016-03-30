function randomInt(n) {
  return Math.floor(Math.random() * n);
}

var fm = [];
function factorial (n) {
  if (n == 0 || n == 1)
    return 1;
  if (fm[n] > 0)
    return fm[n];
  return fm[n] = factorial(n-1) * n;
}

function choose(n, k) {
  var r = 1;
  for (var i = 0; i < k; i++) r *= (n-i);
  r /= factorial(k);
  return r;
}

// var colour = d3.scale.linear()
//   .domain([0, 0.5, 1])
//   .range(["red", "green", "blue"]);
var colour = d3.scale.linear()
  .domain([0, 0.2, 0.4, 0.6, 0.8, 1])
  .range(["red", "blue", "green", "yellow", "white"]);

// function cogfraction(n) {
//   return (numTriangles - (n.cognitive/params.J)) / (2*numTriangles);
// }

var width = 800,
    height = 600,
    params = {},
    numBeliefs = 0,
    numTriangles = 0,
    nodes = [],
    links = [],
    svg = null,
    force = null,
    active = false,
    cogfraction = null,
    nodeColouring = 0;

function initParams() {
  params = {
    N: parseInt($("input[name=N]").val()), // number of individuals
    M: parseInt($("input[name=M]").val()), // number of concepts
    I: parseFloat($("input[name=I]").val()), // social factor
    J: parseFloat($("input[name=J]").val()), // cognitive factor
    K: parseFloat($("input[name=K]").val())  // mean degree
  };
  numBeliefs = params.M * (params.M - 1) / 2; // choose(params.M, 2);
  numTriangles = params.M * (params.M - 1) * (params.M - 2) / 6; // choose(params.M, 3);
}

// create multidimensional array and assign random values
function initNodes() {
  nodes = [];
  for (var x = 0; x < params.N; x++) {
    var m = [];
    for (var i = 0; i < params.M; i++) m[i] = [];
    for (i = 0; i < params.M; i++) {
      m[i][i] = 0;
      for (var j = i+1; j < params.M; j++) {
        var b = randomInt(3) - 1; // random number between -1 and 1
        m[i][j] = b;
        m[j][i] = b;
      }
    }
    nodes[x] = {
      name: x,
      beliefs: m,
      count: count(m),
      cognitive: cog(m)
    };
  }
}

// assign friends randomly
function initLinks() {
  links = [];
  var p = params.K/params.N;
  for (var x = 0; x < (params.N-1); x++) {
    for (var y = x+1; y < params.N; y++) {
      if (Math.random() < p) {
        links.push({
          source: x,
          target: y,
          shared: shared(nodes[x].beliefs,
                         nodes[y].beliefs)
        });
      }
    }
  }
}

// m is the belief matrix
function count(m) {
  var r = 0;
  for (var i = 0; i < (params.M-1); i++) {
    for (var j = i+1; j < params.M; j++) {
      if (m[i][j] == 1) r++;
    }
  }
  return r / numBeliefs;
}

// computes the individual cognitive energy
// m is the belief matrix
function cog(m) {
  var e = 0;
  for (var i = 0; i < (params.M-2); i++)
    for (var j = i+1; j < (params.M-1); j++)
      for (var k = j+1; k < params.M; k++)
        e -= m[i][j] * m[j][k] * m[k][i];
  e *= params.J;
  return e;
}

var freshId = 0,
    lastId = 0,
    cogstateCount = [],
    cogstateIdOfNode = [],
    idOfCogstate = {};

// m is the belief matrix
function cogstate(m) {
  var v = [];
  for (var i = 0; i < (params.M-1); i++)
    for (var j = i+1; j < params.M; j++)
      v.push(m[i][j]);
  return v;
}

function initCogstates() {
  freshId = 0;
  lastId = 0;
  cogstateCount = [];
  cogstateIdOfNode = [];
  idOfCogstate = {};
  for (var x = 0; x < params.N; x++) {
    var v = cogstate(nodes[x].beliefs);
    var id = freshId;
    if (v in idOfCogstate) {
      id = idOfCogstate[v];
      cogstateCount[id] += 1;
    } else {
      freshId += 1;
      idOfCogstate[v] = id;
      cogstateCount[id] = 1;
    }
    cogstateIdOfNode[x] = id;
  }
}

function mostPopular() {
  var max = 0;
  var ids = [];
  for (var i in cogstateCount) {
    var n = cogstateCount[i];
    if (n > max) {
      max = n;
      ids = [i];
    } else if (n == max) {
      ids.push(i);
    }
  }
  return ids;
}

// n and m are belief matrices
function shared(n, m) {
  var r = 0;
  for (var i = 0; i < (params.M-1); i++) {
    for (var j = i+1; j < params.M; j++) {
      if (n[i][j] == m[i][j]) r++;
    }
  }
  return r / numBeliefs;
}

// step functions
function randomBelief() {
  var x = randomInt(params.N),
      i = randomInt(params.M),
      j = randomInt(params.M);
  while (j == i) j = randomInt(params.M);
  var w = nodes[x].beliefs[i][j],
      u = randomInt(2);
  if (u == w) u = -1;
  return { node: x, i: i, j: j, oldvalue: w, newvalue: u };
}

function updateBelief(x, i, j, v) {
  var sediff = 0.0, // social energy difference
      cediff = 0.0; // cognitive energy difference

  for (var k = 0; k < params.M; k++) {
    if ((k != i) && (k != j))
      cediff += nodes[x].beliefs[i][j] *
                nodes[x].beliefs[j][k] *
                nodes[x].beliefs[k][i];
  }
  for (k = 0; k < links.length; k++) {
    if ((links[k].source == nodes[x]) ||
        (links[k].target == nodes[x]))
      sediff += links[k].source.beliefs[i][j] *
                links[k].target.beliefs[i][j] * 2;
  }

  nodes[x].beliefs[i][j] = v;
  nodes[x].beliefs[j][i] = v;

  for (k = 0; k < params.M; k++) {
    if ((k != i) && (k != j))
      cediff -= nodes[x].beliefs[i][j] *
                nodes[x].beliefs[j][k] *
                nodes[x].beliefs[k][i];
  }
  for (k = 0; k < links.length; k++) {
    if ((links[k].source == nodes[x]) ||
        (links[k].target == nodes[x]))
      sediff -= links[k].source.beliefs[i][j] *
                links[k].target.beliefs[i][j] * 2;
  }

  return { social: sediff * params.I, cognitive: cediff * params.J,
           total: sediff * params.I + cediff * params.J };
}

function step() {
  var rnd = randomBelief(),
      energydiff = updateBelief(rnd.node, rnd.i, rnd.j, rnd.newvalue),
      successful = true;

  if ((energydiff.total > 0) &&
      (Math.random() > Math.exp(-energydiff.total))) {
    // backtrack
    updateBelief(rnd.node, rnd.i, rnd.j, rnd.oldvalue);
    successful = false;
  }

  var diff = energydiff;
  // merge rnd and energydiff into res and add successful
  for (var k in rnd) { diff[k] = rnd[k]; }
  diff.successful = successful;
  return diff;
}

// GUI callbacks
function start() {
  active = true;
  nextEvent();
}

function nextEvent() {
  if (active) {
    var rr = parseInt($("input[name=refresh-rate]").val());
    var diff = step();
    while (rr != 0) {
      while (diff.successful == false) {
        // for (var k = 0; k < links.length; k++) {
        //   if ((links[k].source == nodes[diff.node]) ||
        //       (links[k].target == nodes[diff.node])) {
        //     var s = shared(links[k].source.beliefs,
        //                    links[k].target.beliefs);
        //     if ((links[k].shared < s-(1e-6)) || (links[k].shared > s+(1e-6))) {
        //       console.log("!!! " + links[k].shared + " != " + s + " !!!");
        //       console.log(diff);
        //       console.log(links[k].source.name + ": " + links[k].source.beliefs[diff.i][diff.j]);
        //       console.log(links[k].target.name + ": " + links[k].target.beliefs[diff.i][diff.j]);
        //       return;
        //     }
        //   }
        // }
        diff = step();
      }

      // update links
      for (var k = 0; k < links.length; k++) {
        if (links[k].source == nodes[diff.node]) {
          // console.log(links[k].source.name + ", " + links[k].target.name + ", " + links[k].shared);
          if (diff.newvalue == links[k].target.beliefs[diff.i][diff.j])
            links[k].shared += 1 / numBeliefs;
          else if (diff.oldvalue == links[k].target.beliefs[diff.i][diff.j])
            links[k].shared -= 1 / numBeliefs;
          // console.log(links[k].source.name + ", " + links[k].target.name + ", " + links[k].shared);
          // var s = shared(links[k].source.beliefs,
          //                links[k].target.beliefs);
          // if ((links[k].shared < s-(1e-6)) || (links[k].shared > s+(1e-6))) {
          //   console.log("!!! " + links[k].shared + " != " + s + " !!!");
          //   console.log(links[k].source.beliefs[diff.i][diff.j]);
          //   console.log(links[k].target.beliefs[diff.i][diff.j]);
          //   return;
          // }
        }
        if (links[k].target == nodes[diff.node]) {
          // console.log(links[k].source.name + ", " + links[k].target.name + ", " + links[k].shared);
          if (diff.newvalue == links[k].source.beliefs[diff.i][diff.j])
            links[k].shared += 1 / numBeliefs;
          else if (diff.oldvalue == links[k].source.beliefs[diff.i][diff.j])
            links[k].shared -= 1 / numBeliefs;
          // console.log(links[k].source.name + ", " + links[k].target.name + ", " + links[k].shared);
          // var s = shared(links[k].source.beliefs,
          //                links[k].target.beliefs);
          // if ((links[k].shared < s-(1e-6)) || (links[k].shared > s+(1e-6))) {
          //   console.log("!!! " + links[k].shared + " != " + s + " !!!");
          //   console.log(links[k].source.beliefs[diff.i][diff.j]);
          //   console.log(links[k].target.beliefs[diff.i][diff.j]);
          //   return;
          // }
        }
        // if ((links[k].source == nodes[diff.node]) ||
        //     (links[k].target == nodes[diff.node])) {
        //   console.log(links[k].source.name + ", " + links[k].target.name + ", " + links[k].shared);
        //   if (links[k].source.beliefs[diff.i][diff.j] ==
        //       links[k].target.beliefs[diff.i][diff.j])
        //     links[k].shared += 1 / numBeliefs;
        //   else links[k].shared -= 1 / numBeliefs;
        //   console.log(links[k].source.name + ", " + links[k].target.name + ", " + links[k].shared);
        // }
      }

      rr -= 1;
    }
    waitGUI(diff);
  }
}

function waitGUI(diff) {
  // if (force.alpha() > 0.028) {
  //   setTimeout(function() { waitGUI(diff); }, 200);
  // } else {
  //   updateGUI(diff);
  // }
  var delay = parseFloat($("input[name=delay]").val());
  setTimeout(function() { updateGUI(diff); }, delay);
}

function updateGUI(diff) {
  console.log(diff);

  svg.selectAll(".link")
    .style("stroke-width", linkWidth)
    .select("title").text(linkText);

  // update graph
  force
    .linkDistance(linkDistance)
    .linkStrength(linkStrength)
    .alpha(.03);

  // update nodes
  if (diff.newvalue == 1)
    nodes[diff.node].count += 1 / numBeliefs;
  else if (diff.oldvalue == 1)
    nodes[diff.node].count -= 1 / numBeliefs;
  nodes[diff.node].cognitive += diff.cognitive;
  svg.selectAll(".node")
    .style("fill", nodeColour);

  // continue
  nextEvent();
}

function stop() {
  active = false;
}

function nodeColour(n) {
  if (nodeColouring == 0)
    return colour(cogfraction(n.cognitive));
  else if (nodeColouring == 1)
    return colour(n.count);
  else
    return colour(0);
}

function colourByCog() {
  nodeColouring = 0;
  $("#colour-by-cog").addClass('active');
  $("#colour-by-count").removeClass('active');
  svg.selectAll(".node")
    .style("fill", nodeColour);
}

function colourByCount() {
  nodeColouring = 1;
  $("#colour-by-count").addClass('active');
  $("#colour-by-cog").removeClass('active');
  svg.selectAll(".node")
    .style("fill", nodeColour);
}

function linkWidth(l) {
  var s = l.shared;
  return 0.5+s*s*s*10;
}

function linkDistance(l) {
  var d = 1-l.shared;
  return 20+(180*d*d*d);
}

function linkStrength(l) {
  return l.shared;
}

function linkText(l) {
  return l.shared;
}

function nodeText(n) {
  return n.name;
}

function restart() {
  initParams();
  initNodes(params.N, params.M);
  initLinks(params.N, params.K);
  initCogstates();

  cogfraction = d3.scale.linear()
    .domain([-numTriangles*params.J, numTriangles*params.J])
    .range([1, 0]);

  force = d3.layout.force()
    .charge(-80)
    .linkDistance(linkDistance)
    .linkStrength(linkStrength)
    .size([width, height])
    .nodes(nodes)
    .links(links)
    .start();

  svg.empty();

  var link = svg.selectAll(".link")
    .data(links)
    .enter()
    .append("line")
    .attr("class", "link")
    .style("stroke-width", linkWidth);

  link.append("title").text(linkText);

  var node = svg.selectAll(".node")
    .data(nodes)
    .enter()
    .append("circle")
    .attr("class", "node")
    .attr("r", 5)
    .style("fill", nodeColour)
    .style("stroke", "black")
    .style("stroke-width", "0.5px")
    .on("click", function(n) { console.log(n); })
    .call(force.drag);

  node.append("title").text(nodeText);

  force.on("tick", function() {
    link.attr("x1", function(l) { return l.source.x; })
        .attr("y1", function(l) { return l.source.y; })
        .attr("x2", function(l) { return l.target.x; })
        .attr("y2", function(l) { return l.target.y; });

    node.attr("cx", function(n) { return n.x; })
        .attr("cy", function(n) { return n.y; });
  });
}

$(document).ready(function() {
  svg = d3.select("#svg-div").append("svg")
    .style("border-radius", "5px")
    .style("border", "2px solid #73AD21")
    .attr("width", "100%")
    .attr("height", height);
  width = svg.node().getBoundingClientRect().width;
  restart();
});

// TODO: how do we plot the results of many simulations?
function plot() {}
