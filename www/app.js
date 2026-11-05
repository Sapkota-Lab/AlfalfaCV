/* AlfalfaCV workspace interaction: sidebar, ROI, ruler calibration. */
(function () {
  if (window.__alfalfacvUi) {
    return;
  }
  window.__alfalfacvUi = true;

  var SIDEBAR_KEY = "alfalfacv.sidebarCollapsed";

  function applySidebar(collapsed) {
    var shell = document.querySelector(".app-shell");
    if (!shell) {
      return;
    }
    shell.classList.toggle("sidebar-collapsed", collapsed);
    var collapseBtn = document.getElementById("sidebar_collapse");
    if (collapseBtn) {
      collapseBtn.setAttribute("aria-expanded", collapsed ? "false" : "true");
    }
  }

  function hideMenu() {
    var menu = document.getElementById("roi_menu");
    if (menu) {
      menu.classList.remove("is-open");
    }
  }

  function stageImage(stage) {
    return stage ? stage.querySelector("img") : null;
  }

  function syncStage(stage) {
    if (!stage) {
      return;
    }
    var img = stageImage(stage);
    var layer = stage.querySelector(".interaction-layer");
    if (!img || !layer) {
      return;
    }
    var stageRect = stage.getBoundingClientRect();
    var imgRect = img.getBoundingClientRect();
    if (imgRect.width < 2 || imgRect.height < 2) {
      return;
    }
    layer.style.left = (imgRect.left - stageRect.left) + "px";
    layer.style.top = (imgRect.top - stageRect.top) + "px";
    layer.style.width = imgRect.width + "px";
    layer.style.height = imgRect.height + "px";
    layer.style.right = "auto";
    layer.style.bottom = "auto";
  }

  var ROI_PREVIEW_MAX_HEIGHT = 420;

  function fitRoiPreview() {
    var stage = document.getElementById("roi_plot_stage");
    if (!stage) {
      return;
    }
    var img = stage.querySelector("img");
    var output = stage.querySelector(".shiny-plot-output");
    if (!img || !(img.naturalWidth > 0) || !(img.naturalHeight > 0)) {
      return;
    }
    var width = stage.clientWidth;
    if (!(width > 2)) {
      return;
    }
    // Full-width height. Taller crops are contained inside the 420px stage.
    var displayH = width * (img.naturalHeight / img.naturalWidth);
    var tall = displayH > ROI_PREVIEW_MAX_HEIGHT + 1;
    stage.classList.toggle("is-tall", tall);
    img.style.top = "";
    img.style.left = "";
    img.style.position = "";
    if (!tall && output) {
      output.style.height = "";
      output.style.maxHeight = "";
    }
  }

  function syncStages() {
    document.querySelectorAll(".plot-stage").forEach(syncStage);
    fitRoiPreview();
  }

  // Preview-pixel coordinates. The plot device matches the preview raster
  // and CSS scales it uniformly, so a fraction of the displayed image is a
  // fraction of preview pixels — not a raw CSS distance.
  function previewPoint(event, stage) {
    var img = stageImage(stage) || stage;
    var rect = img.getBoundingClientRect();
    var pw = Number(stage.dataset.pw);
    var ph = Number(stage.dataset.ph);
    if (!rect.width || !rect.height || !(pw > 0) || !(ph > 0)) {
      return null;
    }
    var nx = (event.clientX - rect.left) / rect.width;
    var ny = (event.clientY - rect.top) / rect.height;
    nx = Math.min(1, Math.max(0, nx));
    ny = Math.min(1, Math.max(0, ny));
    return { x: nx * pw, y: ny * ph };
  }

  function boxRect(box) {
    return {
      xmin: Number(box.dataset.xmin),
      ymin: Number(box.dataset.ymin),
      xmax: Number(box.dataset.xmax),
      ymax: Number(box.dataset.ymax)
    };
  }

  function paintBox(box, rect, pw, ph) {
    box.style.left = (100 * rect.xmin / pw) + "%";
    box.style.top = (100 * rect.ymin / ph) + "%";
    box.style.width = (100 * (rect.xmax - rect.xmin) / pw) + "%";
    box.style.height = (100 * (rect.ymax - rect.ymin) / ph) + "%";
  }

  function clampRect(rect, pw, ph) {
    function clamp(v, max) {
      return Math.min(max, Math.max(0, v));
    }
    var xmin = clamp(Math.min(rect.xmin, rect.xmax), pw);
    var xmax = clamp(Math.max(rect.xmin, rect.xmax), pw);
    var ymin = clamp(Math.min(rect.ymin, rect.ymax), ph);
    var ymax = clamp(Math.max(rect.ymin, rect.ymax), ph);
    return { xmin: xmin, xmax: xmax, ymin: ymin, ymax: ymax };
  }

  function sendBrush(rect) {
    if (!window.Shiny) {
      return;
    }
    if (Math.abs(rect.xmax - rect.xmin) < 2 || Math.abs(rect.ymax - rect.ymin) < 2) {
      return;
    }
    Shiny.setInputValue("full_preview_brush", {
      xmin: rect.xmin,
      xmax: rect.xmax,
      ymin: rect.ymin,
      ymax: rect.ymax,
      nonce: Date.now()
    }, { priority: "event" });
  }

  var drag = null;
  var scrollGuardUntil = 0;

  function noteWorkspaceScroll() {
    scrollGuardUntil = Date.now() + 280;
    if (drag && drag.draft && drag.draft.parentNode) {
      drag.draft.parentNode.removeChild(drag.draft);
    }
    drag = null;
  }

  function scrollJustHappened() {
    return Date.now() < scrollGuardUntil;
  }

  function onPointerDown(event) {
    if (event.button !== 0 || scrollJustHappened()) {
      return;
    }
    var stage = event.target.closest("#full_plot_stage");
    if (!stage) {
      return;
    }
    hideMenu();
    var pw = Number(stage.dataset.pw);
    var ph = Number(stage.dataset.ph);
    var p = previewPoint(event, stage);
    if (!p) {
      return;
    }
    var box = stage.querySelector(".roi-box");
    var handle = event.target.closest(".roi-handle");
    var onBox = event.target.closest(".roi-box");
    event.preventDefault();
    if (handle && box) {
      drag = {
        mode: "resize",
        handle: handle.dataset.handle,
        stage: stage,
        box: box,
        start: boxRect(box),
        pw: pw,
        ph: ph
      };
    } else if (onBox && box) {
      drag = {
        mode: "move",
        stage: stage,
        box: box,
        start: boxRect(box),
        origin: p,
        pw: pw,
        ph: ph
      };
    } else {
      var layer = stage.querySelector(".interaction-layer") || stage;
      var draft = document.createElement("div");
      draft.className = "roi-draft";
      layer.appendChild(draft);
      drag = {
        mode: "draw",
        stage: stage,
        box: box,
        draft: draft,
        anchor: p,
        pw: pw,
        ph: ph
      };
    }
    if (stage.setPointerCapture) {
      try { stage.setPointerCapture(event.pointerId); } catch (err) { /* ignore */ }
    }
  }

  function movedRect(state, point) {
    var start = state.start;
    var dx = point.x - state.origin.x;
    var dy = point.y - state.origin.y;
    return clampRect({
      xmin: start.xmin + dx,
      xmax: start.xmax + dx,
      ymin: start.ymin + dy,
      ymax: start.ymax + dy
    }, state.pw, state.ph);
  }

  function resizedRect(state, point) {
    var start = state.start;
    var rect = {
      xmin: start.xmin,
      xmax: start.xmax,
      ymin: start.ymin,
      ymax: start.ymax
    };
    var h = state.handle;
    if (h.indexOf("w") !== -1) rect.xmin = point.x;
    if (h.indexOf("e") !== -1) rect.xmax = point.x;
    if (h.indexOf("n") !== -1) rect.ymin = point.y;
    if (h.indexOf("s") !== -1) rect.ymax = point.y;
    return clampRect(rect, state.pw, state.ph);
  }

  function paintDraft(draft, rect, pw, ph) {
    draft.style.left = (100 * rect.xmin / pw) + "%";
    draft.style.top = (100 * rect.ymin / ph) + "%";
    draft.style.width = (100 * (rect.xmax - rect.xmin) / pw) + "%";
    draft.style.height = (100 * (rect.ymax - rect.ymin) / ph) + "%";
  }

  function onPointerMove(event) {
    if (drag) {
      var point = previewPoint(event, drag.stage);
      if (!point) {
        return;
      }
      var rect = null;
      if (drag.mode === "move") {
        rect = movedRect(drag, point);
        paintBox(drag.box, rect, drag.pw, drag.ph);
      } else if (drag.mode === "resize") {
        rect = resizedRect(drag, point);
        paintBox(drag.box, rect, drag.pw, drag.ph);
      } else if (drag.mode === "draw" && drag.draft) {
        rect = clampRect({
          xmin: drag.anchor.x,
          xmax: point.x,
          ymin: drag.anchor.y,
          ymax: point.y
        }, drag.pw, drag.ph);
        paintDraft(drag.draft, rect, drag.pw, drag.ph);
      }
      drag.current = rect;
      return;
    }
    drawGuide(event);
  }

  function onPointerUp(event) {
    if (!drag) {
      return;
    }
    var point = previewPoint(event, drag.stage);
    var rect = drag.current;
    if (!rect && point && drag.mode === "draw") {
      rect = clampRect({
        xmin: drag.anchor.x,
        xmax: point.x,
        ymin: drag.anchor.y,
        ymax: point.y
      }, drag.pw, drag.ph);
    }
    if (drag.draft && drag.draft.parentNode) {
      drag.draft.parentNode.removeChild(drag.draft);
    }
    if (rect) {
      sendBrush(rect);
    }
    drag = null;
  }

  function onContextMenu(event) {
    var stage = event.target.closest("#full_plot_stage");
    if (!stage) {
      hideMenu();
      return;
    }
    event.preventDefault();
    var box = stage.querySelector(".roi-box");
    var point = previewPoint(event, stage);
    var onBox = event.target.closest(".roi-box");
    var inside = false;
    if (box && point) {
      var rect = boxRect(box);
      // Half-open, matching preview_point_inside_roi().
      inside = onBox || (
        point.x >= rect.xmin && point.x < rect.xmax &&
        point.y >= rect.ymin && point.y < rect.ymax
      );
    }
    if (!inside) {
      hideMenu();
      return;
    }
    var menu = document.getElementById("roi_menu");
    if (!menu) {
      return;
    }
    menu.style.left = event.clientX + "px";
    menu.style.top = event.clientY + "px";
    menu.classList.add("is-open");
  }

  function drawGuide(event) {
    var stage = document.getElementById("ruler_plot_stage");
    if (!stage) {
      return;
    }
    var layer = stage.querySelector(".interaction-layer");
    if (!layer || layer.dataset.calMode !== "second") {
      return;
    }
    var bounds = stage.getBoundingClientRect();
    if (event.clientX < bounds.left || event.clientX > bounds.right ||
        event.clientY < bounds.top || event.clientY > bounds.bottom) {
      return;
    }
    var pw = Number(stage.dataset.pw);
    var ph = Number(stage.dataset.ph);
    var x1 = Number(layer.dataset.x1);
    var y1 = Number(layer.dataset.y1);
    var point = previewPoint(event, stage);
    if (!point || !isFinite(x1) || !isFinite(y1) || !(pw > 0) || !(ph > 0)) {
      return;
    }
    var line = layer.querySelector(".cal-guide-line");
    if (!line) {
      line = document.createElement("div");
      line.className = "cal-guide-line";
      layer.appendChild(line);
    }
    var width = layer.clientWidth;
    var xA = (x1 / pw) * width;
    var xB = (point.x / pw) * width;
    var y = (y1 / ph) * layer.clientHeight;
    line.style.left = Math.min(xA, xB) + "px";
    line.style.top = (y - 1) + "px";
    line.style.width = Math.max(0, Math.abs(xB - xA)) + "px";
    line.style.background = xB > xA + 0.5 ? "#2c5f2d" : "#8b3a3a";
  }

  function onRulerClick(event) {
    if (event.button !== 0 || scrollJustHappened()) {
      return;
    }
    var stage = event.target.closest("#ruler_plot_stage");
    if (!stage || event.target.closest("[data-input-action]")) {
      return;
    }
    var layer = stage.querySelector(".interaction-layer");
    if (!layer) {
      return;
    }
    var mode = layer.dataset.calMode;
    if (mode !== "first" && mode !== "second") {
      return;
    }
    var point = previewPoint(event, stage);
    if (!point || !window.Shiny) {
      return;
    }
    event.preventDefault();
    Shiny.setInputValue("ruler_cal_click", {
      x: point.x,
      y: point.y,
      nonce: Date.now()
    }, { priority: "event" });
  }

  function onActionClick(event) {
    var button = event.target.closest("[data-input-action]");
    if (!button || !window.Shiny) {
      return;
    }
    event.preventDefault();
    var inputId = button.getAttribute("data-input-action");
    Shiny.setInputValue(inputId, Date.now(), { priority: "event" });
    if (inputId === "reset_roi") {
      hideMenu();
    }
  }

  document.addEventListener("DOMContentLoaded", function () {
    var collapsed = false;
    try {
      collapsed = sessionStorage.getItem(SIDEBAR_KEY) === "1";
    } catch (err) {
      collapsed = false;
    }
    applySidebar(collapsed);

    var shell = document.querySelector(".app-shell");
    if (shell) {
      shell.addEventListener("transitionend", function (event) {
        if (event.propertyName === "grid-template-columns") {
          syncStages();
        }
      });
    }

    var collapseBtn = document.getElementById("sidebar_collapse");
    var expandBtn = document.getElementById("sidebar_expand");
    if (collapseBtn) {
      collapseBtn.addEventListener("click", function () {
        applySidebar(true);
        try { sessionStorage.setItem(SIDEBAR_KEY, "1"); } catch (err) { /* ignore */ }
        requestAnimationFrame(syncStages);
      });
    }
    if (expandBtn) {
      expandBtn.addEventListener("click", function () {
        applySidebar(false);
        try { sessionStorage.setItem(SIDEBAR_KEY, "0"); } catch (err) { /* ignore */ }
        requestAnimationFrame(syncStages);
      });
    }
  });

  document.addEventListener("wheel", function (event) {
    if (event.target.closest && event.target.closest(".pane-workspace")) {
      noteWorkspaceScroll();
    }
  }, { passive: true, capture: true });
  document.addEventListener("pointerdown", onPointerDown);
  document.addEventListener("pointermove", onPointerMove);
  document.addEventListener("pointerup", onPointerUp);
  document.addEventListener("pointercancel", onPointerUp);
  document.addEventListener("contextmenu", onContextMenu);
  document.addEventListener("click", function (event) {
    onActionClick(event);
    onRulerClick(event);
    if (!event.target.closest("#roi_menu") && !event.target.closest("#full_plot_stage")) {
      hideMenu();
    }
  });
  document.addEventListener("keydown", function (event) {
    if (event.key === "Escape") {
      hideMenu();
    }
  });
  document.addEventListener("shiny:value", function (event) {
    var name = event.name || (event.detail && event.detail.name);
    if (name === "full_preview_plot" || name === "roi_preview_plot" ||
        name === "ruler_preview_plot" ||
        name === "roi_overlay_ui" || name === "ruler_overlay_ui" ||
        name === "workspace_ui") {
      requestAnimationFrame(syncStages);
    }
  });
  document.addEventListener("load", function (event) {
    var img = event.target;
    if (img && img.tagName === "IMG" && img.closest && img.closest(".plot-stage")) {
      syncStage(img.closest(".plot-stage"));
      if (img.closest("#roi_plot_stage")) {
        fitRoiPreview();
      }
    }
  }, true);
  window.addEventListener("resize", function () {
    requestAnimationFrame(syncStages);
  });
})();
