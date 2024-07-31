// @ts-check

import * as monaco from "./lib/monaco/editor.api.js";
import { setGlobalApiBase, api as tapi } from "./tapi.js";
import * as TAPI from "./tapi.js";
import { LANGUAGE_ID } from "./lang.js";

setGlobalApiBase("/api");

/** @type {Record<TAPI.slang_ui.monaco.MarkerSeverity, monaco.MarkerSeverity>} */
const SEVERITY_MAP = {
  Error: monaco.MarkerSeverity.Error,
  Hint: monaco.MarkerSeverity.Hint,
  Info: monaco.MarkerSeverity.Info,
  Warning: monaco.MarkerSeverity.Warning,
};

const DEFAULT_PROGRAM = `
method simple(a: Int): Int
  requires a > 0
  ensures result == 12
  ensures result >= 12
{
  assert a >= 0 ;
  match {
    true => return a + 12,
    true => return 12,
  }
}

global x: Int := 2

method f(): Int
  requires y > 2
  ensures true
  modifies x
{
  var z ;
  var y: Int := x + 2 ;
  y := y + 2 ;
  assert y > 2 ;
  break ;
  loop {
    y > 2 => assert true ; break,
    y < 3 => assert false,
    y == 3 => return y,
  }
}

domain List {
  function nil(): List
  function cons(list: List, x: Int): List

  function head(list: List): Int
  function tail(list: List): List

  function length(list: List): Int
  function get(list: List, index: Int): Int

  axiom length(nil()) == 0
  axiom forall list: List, x: Int :: length(cons(list, x)) == length(list) + 1
  axiom forall list: List, index: Int, x: Int :: index == length(list) ==> get(cons(list, x), index) == x
  axiom forall list: List, x: Int :: head(cons(list, x)) == x
  axiom forall list: List, x: Int :: tail(cons(list, x)) == list
}

method sum(list: List): Int
  ensures result == sum_ref(list)
{
  var res: Int := 0 ;
  for i in 0..length(list) {
    res := get(list, i)
  } ;
  return res
}

function sum_rec(list: List): Int {
  length(list) > 0 ? head(list) + sum_rec(tail(list)) : 0
}
`;

/** @typedef {"disconnected" | "connected"} AppState */
/** @type {AppState} */
let state = "disconnected";

/** @typedef {{type: "idle"} | {type: "analyzing"} | {type: "error"} | {type: "analyzed", markers: TAPI.slang_ui.monaco.MarkerData[]}} Analysis */
/** @type {Analysis} */
let analysis = { type: "idle" };

const isConnected = () => state != "disconnected";

/**
 * @param {number} ms
 * @returns {Promise<void>}
 */
const sleep = (ms) => new Promise((res) => setTimeout(() => res(), ms));

const run = async () => {
  const container = document.getElementById("container");
  const statusBar = document.getElementById("status-bar");
  const statusBarText = document.getElementById("status-bar-text");

  if (!container || !statusBar || !statusBarText) return;

  await Promise.all([
    fetch("./themes/dark.json")
      .then((res) => res.json())
      .then((DARK_THEME) => {
        monaco.editor.defineTheme("slang-dark", DARK_THEME);
      }),
    fetch("./themes/light.json")
      .then((res) => res.json())
      .then((DARK_THEME) => {
        monaco.editor.defineTheme("slang-light", DARK_THEME);
      }),
  ]);

  const PROGRAM_STORE_KEY = "slang-program";
  const initialProgram =
    localStorage.getItem(PROGRAM_STORE_KEY) || DEFAULT_PROGRAM.trim() + "\n";

  const editor = monaco.editor.create(container, {
    value: initialProgram,
    language: LANGUAGE_ID,
    theme: "slang-dark",
    minimap: { enabled: false },
  });

  const model = editor.getModel();
  if (!model) return;

  const updateUI = () => {
    console.log("Update UI", { state, analysis });

    statusBarText.textContent = `${state} / ${analysis.type}`;

    const colors = {
      idle: "bg-gray-500",
      checking: "bg-yellow-500",
      checked: "bg-blue-900",
      error: "bg-red-500",
    };

    if (state == "connected") {
      if (analysis.type == "idle") {
        monaco.editor.setModelMarkers(model, "slang", []);
        statusBar.className = colors.idle;
      } else if (analysis.type == "analyzing") {
        monaco.editor.setModelMarkers(model, "slang", []);
        statusBar.className = colors.checking;
      } else if (analysis.type == "error") {
        monaco.editor.setModelMarkers(model, "slang", []);
        statusBar.className = colors.error;
      } else {
        statusBar.className = colors.checked;
        monaco.editor.setModelMarkers(
          model,
          "slang",
          analysis.markers.map((m) => ({
            severity: SEVERITY_MAP[m.severity],
            message: m.message,
            ...m.span,
          }))
        );
      }
    } else if (state == "disconnected") {
      statusBar.className = colors.idle;
      monaco.editor.setModelMarkers(model, "slang", []);
    }
  };

  /** @typedef {{isAborted: boolean, request: ReturnType<typeof tapi.analyze> | null}} AnalysisRequest */
  /** @type {AnalysisRequest} */
  let oldRequest;
  const onChange = async () => {
    /** @type {AnalysisRequest} */
    let currentRequest = { isAborted: false, request: null };
    try {
      if (oldRequest) {
        oldRequest.request?.abort();
        oldRequest.isAborted = true;
      }
      oldRequest = currentRequest;
      monaco.editor.setModelMarkers(model, "slang", []);
      analysis = { type: "analyzing" };
      updateUI();
      await sleep(1000);
      if (currentRequest.isAborted) return;
      const request = tapi.analyze({ file: editor.getValue() });
      updateUI();
      const res = await request.data;
      if (currentRequest.isAborted) return;
      analysis = { type: "analyzed", markers: res.markers };
      updateUI();
    } catch (e) {
      if (currentRequest && !currentRequest.isAborted) {
        analysis = { type: "error" };
        updateUI();
      }
    }
  };

  const editorDidChange = () => {
    localStorage.setItem(PROGRAM_STORE_KEY, editor.getValue());
    onChange();
  };

  editor.onDidChangeModelContent(editorDidChange);

  let firstTime = true;

  /** @param {AppState | ((old: AppState) => AppState)} newState */
  const setState = (newState) => {
    newState = typeof newState == "function" ? newState(state) : newState;
    const wasDisconnected = !isConnected();
    const changed = newState != state;
    state = newState;

    if (wasDisconnected && isConnected()) {
      if (!firstTime) {
        window.location.reload();
      }
      firstTime = false;
      onChange();
    }

    if (changed) {
      updateUI();
    }
  };

  tapi.heartbeat("Alive", {}).listen((msg) => {
    switch (msg.type) {
      case "error": {
        setState("disconnected");
        break;
      }
      case "open": {
        setState("connected");
        break;
      }
      case "message": {
        setState((prev) => (prev == "disconnected" ? "connected" : prev));
        break;
      }
    }
  });
};

monaco.languages.registerHoverProvider(LANGUAGE_ID, {
  async provideHover(model, position) {
    const result = await tapi.hover({
      file: model.getValue(),
      pos: {
        column: position.column,
        lineNumber: position.lineNumber,
      },
    }).data;

    if (result) {
      return {
        range: result.span,
        contents: result.contents.map((value) => ({ value })),
      };
    }
  },
});

run().catch(console.error);
