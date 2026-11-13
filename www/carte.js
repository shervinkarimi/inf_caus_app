// =====================================================================
// carte.js : carte Leaflet pilotée par le serveur Shiny
// Remplace le package R leaflet (et sf, raster, terra qu'il impose),
// ce qui allège fortement la version en ligne (Shinylive).
// Messages reçus du serveur : carte_etabs, carte_depts, carte_sel.
// Envoi au serveur : input$carte_clic = identifiant cliqué (FI ou code dép.).
// =====================================================================
(function () {
  var BLEU = "#2039b9", FRANCE = [[41.3, -5.2], [51.1, 9.6]];
  var map = null, rendu = null, couchePoints = null, coucheDepts = null, coucheSel = null;
  var geoDepts = null, attente = [], deptSel = null;

  function initCarte() {
    if (map) return true;
    var el = document.getElementById("carte");
    if (!el || typeof L === "undefined") return false;
    map = L.map(el, { minZoom: 4, zoomSnap: 0.25, zoomDelta: 0.5 });
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      maxZoom: 19,
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
    map.createPane("selection").style.zIndex = 650;
    rendu = L.canvas({ padding: 0.5 });        // canvas : rapide avec 2 000 points
    window.carteSCM = map;                      // accès depuis la console du navigateur (débogage)
    map.fitBounds(FRANCE);
    // la carte peut changer de largeur (fenêtre, téléphone tourné)
    new ResizeObserver(function () { map.invalidateSize(); }).observe(el);
    fetch("departements.geojson").then(function (r) { return r.json(); })
      .then(function (g) { geoDepts = g; vider(); });
    return true;
  }

  function clic(id) {
    Shiny.setInputValue("carte_clic", id, { priority: "event" });
  }

  // Messages arrivés avant que la carte ou les contours soient prêts
  function vider() {
    while (attente.length) {
      var m = attente[0];
      if (m.f.besoinGeo && !geoDepts) return;
      attente.shift(); m.f(m.x);
    }
  }
  function quand(f) {
    return function (x) {
      // file d'attente tant que la carte n'est pas prête ; l'ordre des messages est conservé
      if (!initCarte() || (f.besoinGeo && !geoDepts) || attente.length) { attente.push({ f: f, x: x }); return; }
      f(x);
    };
  }

  function retirer() {
    if (couchePoints) { map.removeLayer(couchePoints); couchePoints = null; }
    if (coucheDepts) { map.removeLayer(coucheDepts); coucheDepts = null; }
    if (coucheSel) { map.removeLayer(coucheSel); coucheSel = null; }
  }

  // Établissements : un point par FI ; x = {id, lat, lon, label, cadre}
  function etabs(x) {
    retirer();
    couchePoints = L.layerGroup();
    for (var i = 0; i < x.id.length; i++) {
      var m = L.circleMarker([x.lat[i], x.lon[i]], {
        renderer: rendu, radius: 5, weight: 1, color: "#5b6068",
        fillColor: "#9aa0a8", fillOpacity: 0.7
      });
      m.bindTooltip(x.label[i]);
      (function (id) { m.on("click", function () { clic(id); }); })(x.id[i]);
      couchePoints.addLayer(m);
    }
    couchePoints.addTo(map);
    cadrer(x.cadre);
  }

  function styleDept(f) {
    var s = f.properties.code === deptSel;
    return { color: s ? BLEU : "#7a7f87", weight: s ? 2.5 : 1,
             fillColor: s ? BLEU : "#ffffff", fillOpacity: s ? 0.55 : 0.15 };
  }

  // Départements : x = {sel}
  function depts(x) {
    retirer();
    deptSel = x.sel;
    coucheDepts = L.geoJSON(geoDepts, {
      style: styleDept,
      onEachFeature: function (f, l) {
        l.bindTooltip(f.properties.code + " · " + f.properties.nom, { sticky: true });
        l.on("click", function () { clic(f.properties.code); });
        l.on("mouseover", function () { l.setStyle({ weight: 2.5, color: BLEU }); });
        l.on("mouseout", function () { coucheDepts.resetStyle(l); });
      }
    }).addTo(map);
    map.fitBounds(FRANCE);
  }
  depts.besoinGeo = true;

  // Unité traitée : x = {type: "etab", lat, lon, label, recentrer} ou {type: "dept", code}
  function sel(x) {
    if (x.type === "dept") {
      deptSel = x.code;
      if (coucheDepts) coucheDepts.setStyle(styleDept);
      return;
    }
    if (coucheSel) { map.removeLayer(coucheSel); coucheSel = null; }
    if (x.lat === null || x.lat === undefined) return;
    coucheSel = L.circleMarker([x.lat, x.lon], {
      pane: "selection", radius: 10, color: "#ffffff", weight: 2,
      fillColor: BLEU, fillOpacity: 1
    }).bindTooltip(x.label).addTo(map);
    if (x.recentrer && !map.getBounds().contains([x.lat, x.lon])) map.setView([x.lat, x.lon], 9);
  }

  // cadre : null = ne pas bouger ; [lat, lon] = un seul point ; [[s, o], [n, e]] = zone
  function cadrer(c) {
    if (!c) return;
    if (typeof c[0] === "number") map.setView(c, 11);
    else map.fitBounds(c);
  }

  Shiny.addCustomMessageHandler("carte_etabs", quand(etabs));
  Shiny.addCustomMessageHandler("carte_depts", quand(depts));
  Shiny.addCustomMessageHandler("carte_sel", quand(sel));
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", function () { initCarte(); vider(); });
  else { initCarte(); }
})();
