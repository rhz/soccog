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

var width = 800,
    height = 600,
    params = {},
    numBeliefs = 0,
    numTriangles = 0,
    nodes = [],
    links = [],
    svg = null,
    force = null,
    active = false;

function initParams() {
  params = { N: parseInt($("input[name=N]").val()), // number of individuals
             M: parseInt($("input[name=M]").val()), // number of concepts
             I: parseFloat($("input[name=I]").val()), // social factor
             J: parseFloat($("input[name=J]").val()), // cognitive factor
             K: parseFloat($("input[name=K]").val()) }; // mean degree
  numBeliefs = params.M * (params.M - 1) / 2;
  numTriangles = choose(params.M, 3);
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
      // count: count(m),
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
      energydiff = updateBelief(rnd.node, rnd.i, rnd.j, rnd.newvalue);

  if ((energydiff.total > 0) &&
      (Math.random() > Math.exp(-energydiff.total))) {
    // backtrack
    updateBelief(rnd.node, rnd.i, rnd.j, rnd.oldvalue);
    energydiff = { social: 0.0, cognitive: 0.0, total: 0.0 };
  }

  // merge rnd and energydiff
  for (var k in rnd) { energydiff[k] = rnd[k]; }
  return energydiff;
}

// GUI callbacks
function start() {
  active = true;
  nextEvent();
}

function nextEvent() {
  if (active) {
    var diff = step();
    while (diff.total == 0.0) diff = step();
    waitGUI(diff);
  }
}

function waitGUI(diff) {
  // if (force.alpha() > 0.028) {
  //   setTimeout(function() { waitGUI(diff); }, 200);
  // } else {
  //   updateGUI(diff);
  // }
  setTimeout(function() { updateGUI(diff); }, 10);
}

function updateGUI(diff) {
  console.log(diff);

  // update links
  for (var k = 0; k < links.length; k++) {
    if ((links[k].source == diff.node) ||
        (links[k].target == diff.node)) {
      if (nodes[links[k].source].beliefs[i][j] ==
          nodes[links[k].target].beliefs[i][j])
        links.shared += 1 / numBeliefs;
      else links.shared -= 1 / numBeliefs;
    }
  }
  svg.selectAll(".link")
    .style("stroke-width", linkWidth)
    .select("title").text(linkText);

  // update graph
  force
    .linkDistance(linkDistance)
    .linkStrength(linkStrength)
    .alpha(.03);

  // update nodes
  // if (diff.newvalue == 1)
  //   nodes[diff.node].count += 1 / numBeliefs;
  // else if (diff.oldvalue == 1)
  //   nodes[diff.node].count -= 1 / numBeliefs;
  nodes[diff.node].cognitive += diff.cognitive;
  svg.selectAll(".node")
    .style("fill", nodeColour)
    .select("title").text(nodeText);

  // continue
  nextEvent();
}

function stop() {
  active = false;
}

function nodeColour(n) {
  return colour((numTriangles - (n.cognitive / params.J)) /
                (2 * numTriangles));
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
  return n.cognitive;//name;
}

function restart() {
  initParams();
  initNodes(params.N, params.M);
  initLinks(params.N, params.K);
  initCogstates();

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

