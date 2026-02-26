#pragma once

#include <string>

const std::string FRONTEND_HTML = R"html(<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Copybook Visual Designer</title>
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    background: #0f0f1a; color: #e0e0e0; overflow: hidden;
    height: 100vh; display: flex; flex-direction: column;
}

/* ── Toolbar ────────────────────────────────────────────────────── */
#toolbar {
    display: flex; align-items: center; gap: 6px;
    padding: 6px 16px; background: #1a1a2e;
    border-bottom: 2px solid #0f3460; height: 48px; flex-shrink: 0;
}
#toolbar h1 {
    font-size: 15px; color: #e94560; margin-right: 12px;
    white-space: nowrap; letter-spacing: 0.5px;
}
.tb-sep { width: 1px; height: 28px; background: #0f3460; margin: 0 6px; }
.btn {
    padding: 5px 12px; border: 1px solid #0f3460; border-radius: 4px;
    font-size: 12px; cursor: pointer; transition: all 0.15s;
    background: #16213e; color: #c0c0c0; white-space: nowrap;
}
.btn:hover { background: #e94560; color: #fff; border-color: #e94560; }
.btn-accent { background: #0f3460; color: #5dade2; border-color: #3d7dd8; }
.btn-accent:hover { background: #e94560; color: #fff; border-color: #e94560; }

/* ── SVG Canvas ─────────────────────────────────────────────────── */
#canvas-wrap { flex: 1; position: relative; overflow: hidden; }
#diagram { width: 100%; height: 100%; }
.connection { fill: none; stroke: #5dade2; stroke-width: 1.5; }
.arrow-line { fill: none; stroke: #5dade2; stroke-width: 1.5; marker-end: url(#arrowhead); }

/* ── Modals ─────────────────────────────────────────────────────── */
.modal-overlay {
    display: none; position: fixed; inset: 0;
    background: rgba(0,0,0,0.65); z-index: 1000;
    justify-content: center; align-items: center;
}
.modal-overlay.active { display: flex; }
.modal {
    background: #16213e; border: 1px solid #0f3460; border-radius: 10px;
    width: 90%; max-width: 820px; max-height: 85vh;
    display: flex; flex-direction: column;
    box-shadow: 0 12px 40px rgba(0,0,0,0.6);
    animation: modalIn 0.15s ease-out;
}
@keyframes modalIn { from { transform: scale(0.95); opacity: 0; } to { transform: scale(1); opacity: 1; } }
.modal-head {
    display: flex; justify-content: space-between; align-items: center;
    padding: 14px 20px; border-bottom: 1px solid #0f3460;
}
.modal-head h2 { color: #e94560; font-size: 16px; }
.modal-head .sub { color: #888; font-size: 12px; margin-top: 2px; }
.modal-close {
    background: none; border: none; color: #888; font-size: 22px;
    cursor: pointer; padding: 0 4px; line-height: 1;
}
.modal-close:hover { color: #e94560; }
.modal-body { padding: 16px 20px; overflow: auto; flex: 1; }

/* ── Field Table ────────────────────────────────────────────────── */
.field-table { width: 100%; border-collapse: collapse; font-size: 12.5px; }
.field-table th {
    text-align: left; padding: 7px 10px; background: #0f3460;
    color: #e94560; font-weight: 600; border-bottom: 2px solid #e94560;
    position: sticky; top: 0;
}
.field-table td { padding: 5px 10px; border-bottom: 1px solid #0d0d1a; }
.field-table tr:hover td { background: #1a2a4a; }
.field-table .group-row td { background: #0f1f3a; font-weight: 600; }
.field-table .child-row td:first-child { padding-left: 28px; }
.type-badge {
    display: inline-block; padding: 1px 6px; border-radius: 3px;
    font-size: 10.5px; font-weight: 700; letter-spacing: 0.3px;
}
.t-CHARACTER { background: #27ae60; color: #fff; }
.t-ZONED_NUMERIC, .t-ZONED_UNSIGNED { background: #2980b9; color: #fff; }
.t-PACKED_DECIMAL { background: #8e44ad; color: #fff; }
.t-BINARY { background: #d35400; color: #fff; }
.t-RECORD { background: #e94560; color: #fff; }
.t-FILLER { background: #7f8c8d; color: #fff; }

/* ── Code Viewer ────────────────────────────────────────────────── */
.code-viewer {
    background: #0d1117; border: 1px solid #30363d; border-radius: 6px;
    padding: 14px; font-family: 'Courier New', monospace; font-size: 12.5px;
    color: #c9d1d9; white-space: pre; overflow: auto; max-height: 60vh;
    line-height: 1.5; tab-size: 4;
}
.import-area {
    width: 100%; min-height: 180px; background: #0d1117;
    border: 1px solid #30363d; border-radius: 6px; padding: 12px;
    font-family: 'Courier New', monospace; font-size: 12.5px;
    color: #c9d1d9; resize: vertical;
}
.modal-actions { padding: 12px 20px; border-top: 1px solid #0f3460; text-align: right; }
.modal-actions .btn { margin-left: 8px; }

/* ── Status Bar ─────────────────────────────────────────────────── */
#statusbar {
    height: 22px; background: #0f3460; padding: 0 16px;
    display: flex; align-items: center; font-size: 11px; color: #8899aa;
    flex-shrink: 0;
}
#statusbar .sep { margin: 0 10px; color: #1a3060; }
</style>
</head>
<body>

<!-- ── Toolbar ──────────────────────────────────────────────────── -->
<header id="toolbar">
    <h1>Copybook Designer</h1>
    <div class="tb-sep"></div>
    <button class="btn btn-accent" onclick="loadCopybooks()" title="Reload all .cpy files">Reload Copybooks</button>
    <button class="btn" onclick="showImportDialog()" title="Import .cpy source text">Import Copybook</button>
    <div class="tb-sep"></div>
    <button class="btn btn-accent" onclick="doGenerate()" title="Generate C++ header for selected class">Generate C++</button>
    <button class="btn" onclick="doExport('json')" title="Export schema as JSON">Export JSON</button>
    <button class="btn" onclick="doExport('yaml')" title="Export schema as YAML">Export YAML</button>
    <div class="tb-sep"></div>
    <span id="sel-label" style="font-size:12px;color:#888;">No class selected</span>
</header>

<!-- ── SVG Diagram ─────────────────────────────────────────────── -->
<div id="canvas-wrap">
<svg id="diagram" xmlns="http://www.w3.org/2000/svg">
    <defs>
        <marker id="arrowhead" viewBox="0 0 10 7" refX="9" refY="3.5"
                markerWidth="8" markerHeight="6" orient="auto-start-reverse">
            <polygon points="0 0, 10 3.5, 0 7" fill="#5dade2"/>
        </marker>
        <pattern id="grid" width="24" height="24" patternUnits="userSpaceOnUse">
            <path d="M 24 0 L 0 0 0 24" fill="none" stroke="#161628" stroke-width="0.5"/>
        </pattern>
    </defs>
    <rect width="100%" height="100%" fill="url(#grid)"/>
    <g id="connections-layer"></g>
    <g id="nodes-layer"></g>
</svg>
</div>

<!-- ── Detail Modal (double-click) ─────────────────────────────── -->
<div id="detail-modal" class="modal-overlay">
<div class="modal">
    <div class="modal-head">
        <div><h2 id="detail-title"></h2><div class="sub" id="detail-sub"></div></div>
        <button class="modal-close" onclick="closeModal('detail-modal')">&times;</button>
    </div>
    <div class="modal-body">
        <table class="field-table">
            <thead><tr>
                <th>Field Name</th><th>PIC</th><th>Offset</th>
                <th>Size</th><th>Type</th><th>Decimals</th>
            </tr></thead>
            <tbody id="detail-tbody"></tbody>
        </table>
    </div>
</div>
</div>

<!-- ── Code / Export Modal ─────────────────────────────────────── -->
<div id="code-modal" class="modal-overlay">
<div class="modal">
    <div class="modal-head">
        <div><h2 id="code-title"></h2><div class="sub" id="code-sub"></div></div>
        <button class="modal-close" onclick="closeModal('code-modal')">&times;</button>
    </div>
    <div class="modal-body">
        <pre class="code-viewer" id="code-content"></pre>
    </div>
    <div class="modal-actions">
        <button class="btn" onclick="copyCode()">Copy to Clipboard</button>
    </div>
</div>
</div>

<!-- ── Import Modal ────────────────────────────────────────────── -->
<div id="import-modal" class="modal-overlay">
<div class="modal">
    <div class="modal-head">
        <div><h2>Import Copybook</h2><div class="sub">Paste COBOL copybook (.cpy) source below</div></div>
        <button class="modal-close" onclick="closeModal('import-modal')">&times;</button>
    </div>
    <div class="modal-body">
        <textarea id="import-source" class="import-area" placeholder="       01  RECORD-NAME.
           05  FIELD-1      PIC X(10).
           05  FIELD-2      PIC 9(5)."></textarea>
    </div>
    <div class="modal-actions">
        <button class="btn btn-accent" onclick="doImport()">Parse &amp; Add to Diagram</button>
    </div>
</div>
</div>

<!-- ── Status Bar ──────────────────────────────────────────────── -->
<footer id="statusbar">
    <span id="status-msg">Ready</span>
    <span class="sep">|</span>
    <span id="status-count">0 classes</span>
    <span class="sep">|</span>
    <span>Double-click a class to inspect fields</span>
</footer>

<script>
/* ═══════════════════════════════════════════════════════════════════
   Copybook Visual Designer — Client-Side Logic
   ═══════════════════════════════════════════════════════════════════ */

const SVG_NS = 'http://www.w3.org/2000/svg';
const NODE_W = 220, NODE_H = 58, NODE_R = 6, HDR_H = 30;

let allCopybooks = [];   // raw definitions from server
let nodes = [];          // { id, name, cobolName, totalSize, fields, fieldCount,
                         //   x, y, w, h, isChild, parentId, copybookIdx, el }
let connections = [];    // { fromId, toId, el }
let nodesById = {};
let selectedId = null;
let dragState = null;

/* ── SVG helpers ───────────────────────────────────────────────── */
function svgEl(tag, attrs) {
    const el = document.createElementNS(SVG_NS, tag);
    for (const [k, v] of Object.entries(attrs || {})) el.setAttribute(k, v);
    return el;
}

/* ── Initialization ────────────────────────────────────────────── */
window.addEventListener('load', loadCopybooks);

async function loadCopybooks() {
    setStatus('Loading copybooks...');
    try {
        const resp = await fetch('/api/copybooks');
        const data = await resp.json();
        allCopybooks = data.copybooks || [];
        buildDiagram();
        setStatus('Loaded ' + allCopybooks.length + ' copybooks');
    } catch (e) {
        setStatus('Error: ' + e.message);
    }
}

/* ── Build diagram from parsed copybooks ───────────────────────── */
function buildDiagram() {
    nodes = [];
    connections = [];
    nodesById = {};
    selectedId = null;
    document.getElementById('sel-label').textContent = 'No class selected';

    // Clear SVG layers
    const nodesLayer = document.getElementById('nodes-layer');
    const connsLayer = document.getElementById('connections-layer');
    nodesLayer.innerHTML = '';
    connsLayer.innerHTML = '';

    // Remove old clip paths
    const svg = document.getElementById('diagram');
    svg.querySelectorAll('clipPath[id^="clip-"]').forEach(cp => cp.remove());

    // Create nodes from copybooks
    let px = 100;
    for (let ci = 0; ci < allCopybooks.length; ci++) {
        const cb = allCopybooks[ci];
        const groups = (cb.fields || []).filter(f => f.is_group && !f.is_filler);
        const nonGroupFields = (cb.fields || []).filter(f => !f.is_group);
        const parentId = 'cb-' + ci;

        // Parent node
        const parentNode = {
            id: parentId, name: cb.class_name, cobolName: cb.name,
            totalSize: cb.total_size, fields: cb.fields || [],
            fieldCount: (cb.fields || []).length,
            x: px, y: 80, w: NODE_W, h: NODE_H,
            isChild: false, parentId: null, copybookIdx: ci, el: null
        };
        nodes.push(parentNode);
        nodesById[parentId] = parentNode;

        // Child group nodes
        const childW = groups.length * (NODE_W + 40);
        let cx = px + NODE_W / 2 - childW / 2 + 20;
        for (let gi = 0; gi < groups.length; gi++) {
            const g = groups[gi];
            const childId = parentId + '-g' + gi;
            const childNode = {
                id: childId, name: g.cpp_name || g.name,
                cobolName: g.cobol_name || g.name,
                totalSize: g.size,
                fields: g.children || [],
                fieldCount: (g.children || []).length,
                x: cx, y: 260, w: NODE_W, h: NODE_H,
                isChild: true, parentId: parentId, copybookIdx: ci, el: null
            };
            nodes.push(childNode);
            nodesById[childId] = childNode;
            connections.push({ fromId: parentId, toId: childId, el: null });
            cx += NODE_W + 40;
        }

        px += Math.max(NODE_W + 60, childW + 60);
    }

    // Render
    for (const conn of connections) {
        const path = svgEl('path', { class: 'arrow-line' });
        conn.el = path;
        connsLayer.appendChild(path);
    }
    for (const node of nodes) {
        node.el = createNodeSvg(node);
        nodesLayer.appendChild(node.el);
    }
    updateAllConnections();
    document.getElementById('status-count').textContent = nodes.length + ' classes';
}

/* ── Create SVG group for a class node ─────────────────────────── */
function createNodeSvg(node) {
    const g = svgEl('g', { transform: `translate(${node.x},${node.y})` });

    // Clip path for rounded header
    const svg = document.getElementById('diagram');
    const defs = svg.querySelector('defs');
    const clipId = 'clip-' + node.id.replace(/[^a-zA-Z0-9_]/g, '_');
    const clip = svgEl('clipPath', { id: clipId });
    clip.appendChild(svgEl('rect', { width: node.w, height: node.h, rx: NODE_R }));
    defs.appendChild(clip);

    // Body background + border
    g.appendChild(svgEl('rect', {
        width: node.w, height: node.h, rx: NODE_R,
        fill: '#1e2a3a', stroke: '#3d7dd8', 'stroke-width': '1.5'
    }));
    g._border = g.lastChild;

    // Header fill (clipped)
    g.appendChild(svgEl('rect', {
        width: node.w, height: HDR_H,
        fill: node.isChild ? '#1a5a5a' : '#2a4a7a',
        'clip-path': `url(#${clipId})`
    }));

    // Separator
    g.appendChild(svgEl('line', {
        x1: 0, y1: HDR_H, x2: node.w, y2: HDR_H,
        stroke: '#3d7dd8', 'stroke-width': '0.5'
    }));

    // Title
    const title = svgEl('text', {
        x: node.w / 2, y: 19, 'text-anchor': 'middle',
        fill: node.isChild ? '#5dade2' : '#e94560',
        'font-size': '13px', 'font-weight': 'bold',
        'font-family': "'Segoe UI', sans-serif"
    });
    title.textContent = node.name;
    g.appendChild(title);

    // Subtitle
    const sub = svgEl('text', {
        x: node.w / 2, y: HDR_H + 18, 'text-anchor': 'middle',
        fill: '#8899aa', 'font-size': '11px',
        'font-family': "'Segoe UI', sans-serif"
    });
    sub.textContent = node.fieldCount + ' fields  |  ' + node.totalSize + ' bytes';
    g.appendChild(sub);

    // Interaction
    g.style.cursor = 'grab';
    g.addEventListener('mousedown', e => onNodeMouseDown(e, node));
    g.addEventListener('dblclick', e => { e.stopPropagation(); showDetails(node); });
    g.addEventListener('click', e => { e.stopPropagation(); selectNode(node.id); });

    return g;
}

/* ── Connection arrows ─────────────────────────────────────────── */
function updateAllConnections() {
    for (const c of connections) updateConnection(c);
}
function updateConnection(c) {
    const from = nodesById[c.fromId], to = nodesById[c.toId];
    if (!from || !to) return;
    const x1 = from.x + from.w / 2, y1 = from.y + from.h;
    const x2 = to.x + to.w / 2,     y2 = to.y;
    const dy = Math.abs(y2 - y1) * 0.45;
    c.el.setAttribute('d', `M ${x1} ${y1} C ${x1} ${y1+dy}, ${x2} ${y2-dy}, ${x2} ${y2}`);
}

/* ── Drag handling ─────────────────────────────────────────────── */
function onNodeMouseDown(e, node) {
    if (e.button !== 0) return;
    e.preventDefault();
    e.stopPropagation();
    const svg = document.getElementById('diagram');
    const pt = svg.createSVGPoint();
    pt.x = e.clientX; pt.y = e.clientY;
    const sp = pt.matrixTransform(svg.getScreenCTM().inverse());
    dragState = { node, ox: sp.x - node.x, oy: sp.y - node.y };
    node.el.style.cursor = 'grabbing';
}
document.addEventListener('mousemove', e => {
    if (!dragState) return;
    const svg = document.getElementById('diagram');
    const pt = svg.createSVGPoint();
    pt.x = e.clientX; pt.y = e.clientY;
    const sp = pt.matrixTransform(svg.getScreenCTM().inverse());
    dragState.node.x = sp.x - dragState.ox;
    dragState.node.y = sp.y - dragState.oy;
    dragState.node.el.setAttribute('transform',
        `translate(${dragState.node.x},${dragState.node.y})`);
    updateAllConnections();
});
document.addEventListener('mouseup', () => {
    if (dragState) { dragState.node.el.style.cursor = 'grab'; dragState = null; }
});

/* ── Selection ─────────────────────────────────────────────────── */
function selectNode(id) {
    // Deselect previous
    if (selectedId && nodesById[selectedId]) {
        const prev = nodesById[selectedId];
        prev.el._border.setAttribute('stroke', '#3d7dd8');
        prev.el._border.setAttribute('stroke-width', '1.5');
    }
    selectedId = id;
    const node = nodesById[id];
    if (node) {
        node.el._border.setAttribute('stroke', '#e94560');
        node.el._border.setAttribute('stroke-width', '2.5');
        // Resolve parent copybook for label
        const parentNode = node.isChild ? nodesById[node.parentId] : node;
        document.getElementById('sel-label').textContent =
            'Selected: ' + parentNode.name + (node.isChild ? ' > ' + node.name : '');
    }
}
// Click on empty canvas deselects
document.getElementById('diagram').addEventListener('click', () => {
    if (selectedId && nodesById[selectedId]) {
        nodesById[selectedId].el._border.setAttribute('stroke', '#3d7dd8');
        nodesById[selectedId].el._border.setAttribute('stroke-width', '1.5');
    }
    selectedId = null;
    document.getElementById('sel-label').textContent = 'No class selected';
});

/* ── Detail Dialog (double-click) ──────────────────────────────── */
function showDetails(node) {
    document.getElementById('detail-title').textContent = node.name;
    document.getElementById('detail-sub').textContent =
        'COBOL: ' + node.cobolName + '  |  Size: ' + node.totalSize +
        ' bytes  |  ' + node.fieldCount + ' fields';

    const tbody = document.getElementById('detail-tbody');
    tbody.innerHTML = '';
    renderFieldRows(tbody, node.fields, false);
    openModal('detail-modal');
}
function renderFieldRows(tbody, fields, isChild) {
    for (const f of fields) {
        if (f.is_filler) continue;
        const tr = document.createElement('tr');
        tr.className = f.is_group ? 'group-row' : (isChild ? 'child-row' : '');
        const typeName = f.type || 'CHARACTER';
        tr.innerHTML =
            '<td>' + (f.cpp_name || f.name) + '</td>' +
            '<td><code>' + (f.pic_clause || (f.is_group ? 'GROUP' : '-')) + '</code></td>' +
            '<td>' + f.offset + '</td>' +
            '<td>' + f.size + '</td>' +
            '<td><span class="type-badge t-' + typeName + '">' + typeName + '</span></td>' +
            '<td>' + (f.decimal_positions || '-') + '</td>';
        tbody.appendChild(tr);
        if (f.children && f.children.length > 0) {
            renderFieldRows(tbody, f.children, true);
        }
    }
}

/* ── Generate C++ ──────────────────────────────────────────────── */
async function doGenerate() {
    const idx = getSelectedCopybookIndex();
    if (idx < 0) return alert('Select a class node first.');
    setStatus('Generating C++ code...');
    try {
        const resp = await fetch('/api/generate', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ index: idx })
        });
        const data = await resp.json();
        document.getElementById('code-title').textContent = 'Generated C++ Header';
        document.getElementById('code-sub').textContent =
            allCopybooks[idx].class_name + ' — ' + allCopybooks[idx].total_size + ' bytes';
        document.getElementById('code-content').textContent = data.code || data.error;
        openModal('code-modal');
        setStatus('Code generated');
    } catch (e) { setStatus('Error: ' + e.message); }
}

/* ── Export JSON/YAML ──────────────────────────────────────────── */
async function doExport(fmt) {
    const idx = getSelectedCopybookIndex();
    if (idx < 0) return alert('Select a class node first.');
    setStatus('Exporting ' + fmt.toUpperCase() + '...');
    try {
        const resp = await fetch('/api/export/' + fmt, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ index: idx })
        });
        const data = await resp.json();
        document.getElementById('code-title').textContent =
            'Export ' + fmt.toUpperCase();
        document.getElementById('code-sub').textContent =
            allCopybooks[idx].class_name + ' schema';
        document.getElementById('code-content').textContent = data.output || data.error;
        openModal('code-modal');
        setStatus('Exported ' + fmt.toUpperCase());
    } catch (e) { setStatus('Error: ' + e.message); }
}

/* ── Import Copybook ───────────────────────────────────────────── */
function showImportDialog() { openModal('import-modal'); }
async function doImport() {
    const src = document.getElementById('import-source').value.trim();
    if (!src) return alert('Paste copybook source first.');
    setStatus('Parsing copybook...');
    try {
        const resp = await fetch('/api/parse', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ source: src })
        });
        const data = await resp.json();
        if (data.error) { alert(data.error); setStatus('Parse error'); return; }
        allCopybooks.push(data);
        closeModal('import-modal');
        buildDiagram();
        setStatus('Imported: ' + data.class_name);
    } catch (e) { setStatus('Error: ' + e.message); }
}

/* ── Helpers ────────────────────────────────────────────────────── */
function getSelectedCopybookIndex() {
    if (!selectedId) return -1;
    const node = nodesById[selectedId];
    if (!node) return -1;
    return node.copybookIdx;
}

function openModal(id) { document.getElementById(id).classList.add('active'); }
function closeModal(id) { document.getElementById(id).classList.remove('active'); }

function copyCode() {
    const text = document.getElementById('code-content').textContent;
    navigator.clipboard.writeText(text).then(
        () => setStatus('Copied to clipboard'),
        () => setStatus('Copy failed')
    );
}

function setStatus(msg) { document.getElementById('status-msg').textContent = msg; }

// Keyboard shortcuts
document.addEventListener('keydown', e => {
    if (e.key === 'Escape') {
        document.querySelectorAll('.modal-overlay.active')
            .forEach(m => m.classList.remove('active'));
    }
});
</script>
</body>
</html>
)html";
