function randomInt(n) {
  return Math.floor(Math.random() * n);
}

function randomColour() {
  // return "hsl(" + Math.random()*360 + ",100%,50%)";
  return "rgb(" + randomInt(255) + "," + randomInt(255) + "," + randomInt(255) + ")";
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

var colour = d3.scale.linear()
  .domain([     0,    0.2,   0.4,     0.6,      0.8,       1])
  .range(["black", "blue", "red", "green", "yellow", "white"]);

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
    // lastId = 0,
    cogstateCount = [],
    cogstateIdOfNode = [],
    idOfCogstate = {},
    cogstateColour = [];

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
      cogstateColour.push(randomColour());
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
      cediff = 0.0, // cognitive energy difference
      diff = nodes[x].beliefs[i][j] - v;

  for (var k = 0; k < params.M; k++) {
    if ((k != i) && (k != j))
      cediff += nodes[x].beliefs[j][k] *
                nodes[x].beliefs[k][i];
  }
  for (k = 0; k < links.length; k++) {
    if (links[k].source == nodes[x])
      sediff += links[k].target.beliefs[i][j] * 2;
    else if (links[k].target == nodes[x])
      sediff += links[k].source.beliefs[i][j] * 2;
  }

  nodes[x].beliefs[i][j] = v;
  nodes[x].beliefs[j][i] = v;

  var soc = sediff * params.I * diff,
      cog = cediff * params.J * diff;
  return { social: soc, cognitive: cog, total: soc + cog };
}

var steps = 0,
    fails = 0;

function step() {
  steps++;
  var rnd = randomBelief(),
      energydiff = updateBelief(rnd.node, rnd.i, rnd.j, rnd.newvalue),
      successful = true;

  if ((energydiff.total > 0) &&
      (Math.random() > Math.exp(-energydiff.total))) {
    // backtrack
    updateBelief(rnd.node, rnd.i, rnd.j, rnd.oldvalue);
    successful = false;
  }

  // merge rnd and energydiff into diff and add successful
  var diff = energydiff;
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
        fails++;
        console.assert(diff.total > 0.0,
          "rejection with negative energy difference");
        diff = step();
      }

      // update links
      for (var k = 0; k < links.length; k++) {
        if (links[k].source == nodes[diff.node]) {
          if (diff.newvalue == links[k].target.beliefs[diff.i][diff.j])
            links[k].shared += 1 / numBeliefs;
          else if (diff.oldvalue == links[k].target.beliefs[diff.i][diff.j])
            links[k].shared -= 1 / numBeliefs;
        }
        if (links[k].target == nodes[diff.node]) {
          if (diff.newvalue == links[k].source.beliefs[diff.i][diff.j])
            links[k].shared += 1 / numBeliefs;
          else if (diff.oldvalue == links[k].source.beliefs[diff.i][diff.j])
            links[k].shared -= 1 / numBeliefs;
        }
      }

      // update nodes
      if (diff.newvalue == 1)
        nodes[diff.node].count += 1 / numBeliefs;
      else if (diff.oldvalue == 1)
        nodes[diff.node].count -= 1 / numBeliefs;

      nodes[diff.node].cognitive += diff.cognitive;

      // update cogstates
      var oldcogstate = cogstateIdOfNode[diff.node];
      cogstateCount[oldcogstate] -= 1;
      var v = cogstate(nodes[diff.node].beliefs);
      var id = freshId;
      if (v in idOfCogstate) {
        id = idOfCogstate[v];
        cogstateCount[id] += 1;
      } else { // new cogstate found
        freshId += 1;
        cogstateColour.push(randomColour());
        idOfCogstate[v] = id;
        cogstateCount[id] = 1;
      }
      cogstateIdOfNode[diff.node] = id;

      // update rejection rate plot
      if (steps >= 500) {
        addPoint(fails/steps);
        steps = 0;
        fails = 0;
      }

      // decrease refresh rate
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

  force
    .linkDistance(linkDistance)
    .linkStrength(linkStrength)
    .start();

  svg.selectAll(".link")
    .style("stroke-width", linkWidth)
    .select("title").text(linkText);

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
  else if (nodeColouring == 2)
    return cogstateColour[cogstateIdOfNode[n.name]];
  else
    return colour(0);
}

function colourByCog() {
  nodeColouring = 0;
  $("#colour-by-cog").addClass('active');
  $("#colour-by-count").removeClass('active');
  $("#colour-by-cogstate").removeClass('active');
  svg.selectAll(".node").style("fill", nodeColour);
}

function colourByCount() {
  nodeColouring = 1;
  $("#colour-by-cog").removeClass('active');
  $("#colour-by-count").addClass('active');
  $("#colour-by-cogstate").removeClass('active');
  svg.selectAll(".node").style("fill", nodeColour);
}

function colourByCogstate() {
  nodeColouring = 2;
  $("#colour-by-count").removeClass('active');
  $("#colour-by-cog").removeClass('active');
  $("#colour-by-cogstate").addClass('active');
  svg.selectAll(".node").style("fill", nodeColour);
}

function linkWidth(l) {
  var s = l.shared;
  return 0.5+4*s;
}

function linkDistance(l) {
  var d = 1-l.shared;
  return 1.1*params.N+(40*Math.log(params.N)*d);
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
  active = false;
  initParams();
  initNodes(params.N, params.M);
  initLinks(params.N, params.K);
  initCogstates();

  cogfraction = d3.scale.linear()
    .domain([-numTriangles*params.J, numTriangles*params.J])
    .range([1, 0]);

  np = 0;
  last = 0;

  rej.selectAll(".plotline").remove();
  svg.selectAll(".link").remove();
  svg.selectAll(".node").remove();

  force = d3.layout.force()
    .charge(-80)
    .linkDistance(linkDistance)
    .linkStrength(linkStrength)
    .size([width, height])
    .nodes(nodes)
    .links(links)
    .start();

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

var rej = null,
    rejw = 200,
    rejh = 200,
    xscale = null,
    yscale = null,
    np = 0, // number of points in plot
    last = 0; // last point in plot

$(document).ready(function() {
  // add graph visualisation
  svg = d3.select("#graph-div").append("svg")
    .style("border-radius", "5px")
    .style("border", "2px solid #73AD21")
    .attr("width", "100%")
    .attr("height", height);
  width = svg.node().getBoundingClientRect().width;
  // add rejection rate plot
  var margin = { top: 10, right: 10, bottom: 20, left: 30 };
  rej = d3.select("#rejection-plot-div").append("svg")
    .attr("width", "100%")
    .attr("height", rejh + margin.top + margin.bottom);
  rejw = rej.node().getBoundingClientRect().width - margin.left - margin.right;
  rej = rej.append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  xscale = d3.scale.linear().domain([0, 1]).range([0, rejw]);
  yscale = d3.scale.linear().domain([0, 1]).range([rejh, 0]);
  var xaxis = d3.svg.axis().scale(xscale).ticks(0);
  rej.append("g")
    .attr("class", "axis")
    .attr("transform", "translate(0," + rejh + ")")
    .call(xaxis);
  var yaxis = d3.svg.axis().scale(yscale).ticks(2).orient("left");
  rej.append("g")
    .attr("class", "axis")
    .call(yaxis)
    .append("text")
    .attr("x", 10)
    .style("text-anchor", "start")
    .text("Rejection rate");
  // draw graph
  karate(); // restart();
});

function addPoint(neu) {
  np++;
  rej.selectAll(".plotline")
    .attr("x1", function(d,i) { return xscale(i/np); })
    .attr("x2", function(d,i) { return xscale((i+1)/np); });
  rej.append("line")
    .attr("class", "plotline")
    .attr("x1", xscale((np-1)/np))
    .attr("y1", yscale(last))
    .attr("x2", xscale(1))
    .attr("y2", yscale(neu));
  last = neu;
}

// TODO: it would be nice to have a live histogram of the
// number of individuals in each distinct cognitive state
// and maybe also a matrix that tells you have many shared beliefs
// each pair of distinct cognitive states have.

function karate() {
  active = false;
  initParams();
  params.N = 34;
  initNodes(params.N, params.M);
  initCogstates();

  // make links
  links = [
    { source: 1, target: 2, shared: 0.0 },
    { source: 1, target: 3, shared: 0.0 },
    { source: 2, target: 3, shared: 0.0 },
    { source: 1, target: 4, shared: 0.0 },
    { source: 2, target: 4, shared: 0.0 },
    { source: 3, target: 4, shared: 0.0 },
    { source: 1, target: 5, shared: 0.0 },
    { source: 1, target: 6, shared: 0.0 },
    { source: 1, target: 7, shared: 0.0 },
    { source: 5, target: 7, shared: 0.0 },
    { source: 6, target: 7, shared: 0.0 },
    { source: 1, target: 8, shared: 0.0 },
    { source: 2, target: 8, shared: 0.0 },
    { source: 3, target: 8, shared: 0.0 },
    { source: 4, target: 8, shared: 0.0 },
    { source: 1, target: 9, shared: 0.0 },
    { source: 3, target: 9, shared: 0.0 },
    { source: 3, target: 10, shared: 0.0 },
    { source: 1, target: 11, shared: 0.0 },
    { source: 5, target: 11, shared: 0.0 },
    { source: 6, target: 11, shared: 0.0 },
    { source: 1, target: 12, shared: 0.0 },
    { source: 1, target: 13, shared: 0.0 },
    { source: 4, target: 13, shared: 0.0 },
    { source: 1, target: 14, shared: 0.0 },
    { source: 2, target: 14, shared: 0.0 },
    { source: 3, target: 14, shared: 0.0 },
    { source: 4, target: 14, shared: 0.0 },
    { source: 6, target: 17, shared: 0.0 },
    { source: 7, target: 17, shared: 0.0 },
    { source: 1, target: 18, shared: 0.0 },
    { source: 2, target: 18, shared: 0.0 },
    { source: 1, target: 20, shared: 0.0 },
    { source: 2, target: 20, shared: 0.0 },
    { source: 1, target: 22, shared: 0.0 },
    { source: 2, target: 22, shared: 0.0 },
    { source: 24, target: 26, shared: 0.0 },
    { source: 25, target: 26, shared: 0.0 },
    { source: 3, target: 28, shared: 0.0 },
    { source: 24, target: 28, shared: 0.0 },
    { source: 25, target: 28, shared: 0.0 },
    { source: 3, target: 29, shared: 0.0 },
    { source: 24, target: 30, shared: 0.0 },
    { source: 27, target: 30, shared: 0.0 },
    { source: 2, target: 31, shared: 0.0 },
    { source: 9, target: 31, shared: 0.0 },
    { source: 1, target: 32, shared: 0.0 },
    { source: 25, target: 32, shared: 0.0 },
    { source: 26, target: 32, shared: 0.0 },
    { source: 29, target: 32, shared: 0.0 },
    { source: 3, target: 33, shared: 0.0 },
    { source: 9, target: 33, shared: 0.0 },
    { source: 15, target: 33, shared: 0.0 },
    { source: 16, target: 33, shared: 0.0 },
    { source: 19, target: 33, shared: 0.0 },
    { source: 21, target: 33, shared: 0.0 },
    { source: 23, target: 33, shared: 0.0 },
    { source: 24, target: 33, shared: 0.0 },
    { source: 30, target: 33, shared: 0.0 },
    { source: 31, target: 33, shared: 0.0 },
    { source: 32, target: 33, shared: 0.0 },
    { source: 9, target: 34, shared: 0.0 },
    { source: 10, target: 34, shared: 0.0 },
    { source: 14, target: 34, shared: 0.0 },
    { source: 15, target: 34, shared: 0.0 },
    { source: 16, target: 34, shared: 0.0 },
    { source: 19, target: 34, shared: 0.0 },
    { source: 20, target: 34, shared: 0.0 },
    { source: 21, target: 34, shared: 0.0 },
    { source: 23, target: 34, shared: 0.0 },
    { source: 24, target: 34, shared: 0.0 },
    { source: 27, target: 34, shared: 0.0 },
    { source: 28, target: 34, shared: 0.0 },
    { source: 29, target: 34, shared: 0.0 },
    { source: 30, target: 34, shared: 0.0 },
    { source: 31, target: 34, shared: 0.0 },
    { source: 32, target: 34, shared: 0.0 },
    { source: 33, target: 34, shared: 0.0 }
  ];

  for (var x = 0; x < links.length; x++) {
    var src = links[x].source - 1,
        tgt = links[x].target - 1;
    links[x].source = src;
    links[x].target = tgt;
    links[x].shared = shared(nodes[src].beliefs,
                             nodes[tgt].beliefs);
  }

  cogfraction = d3.scale.linear()
    .domain([-numTriangles*params.J, numTriangles*params.J])
    .range([1, 0]);

  np = 0;
  last = 0;

  rej.selectAll(".plotline").remove();
  svg.selectAll(".link").remove();
  svg.selectAll(".node").remove();

  force = d3.layout.force()
    .charge(-80)
    .linkDistance(linkDistance)
    .linkStrength(linkStrength)
    .size([width, height])
    .nodes(nodes)
    .links(links)
    .start();

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
