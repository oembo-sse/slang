// @ts-check
let GLOBAL_API_BASE = "";
/**
 * @param {ApiOptions} [options]
 * @returns {string}
 */
export const getApiBase = (options) => options?.apiBase ?? GLOBAL_API_BASE;
/**
 * @param {string} apiBase
 * @returns {string}
 */
export const setGlobalApiBase = (apiBase) => (GLOBAL_API_BASE = apiBase);
/**
 * @template Req
 * @template Res
 * @param {RequestType} reqTy
 * @param {Method} method
 * @param {string} path
 * @param {ResponseType} resTy
 * @returns {(req: Req, options?: ApiOptions) => {    data: Promise<Res>;    abort: () => void;}}
 */
const request = (reqTy, method, path, resTy) => (req, options) => {
  const controller = new AbortController();
  try {
    const promise = fetch(`${getApiBase(options)}${path}`, {
      method,
      headers:
        reqTy == "json" ? { "Content-Type": "application/json" } : void 0,
      body: reqTy == "json" ? JSON.stringify(req) : void 0,
      signal: controller.signal,
    });
    return {
      data: (async () => {
        const res = await promise;
        if (!res.ok) throw new Error(await res.text());
        if (resTy == "none") return "";
        if (resTy == "json") return await res.json();
        if (resTy == "text") return await res.text();
        throw new Error(`Unknown response type ${resTy}`);
      })(),
      abort: () => controller.abort(),
    };
  } catch (e) {
    console.error(e);
    return {
      data: Promise.reject(e),
      abort: () => controller.abort(),
    };
  }
};
/**
 * @template T
 * @template P
 * @param {(params: P) => string} url
 * @param {ResponseType} resTy
 * @returns {(params: P, options?: ApiOptions) => { cancel: () => void; listen: (stream: SSEStream<T>) => void; }}
 */
const sse = (url, resTy) => (params, options) => {
  const source = new EventSource(`${getApiBase(options)}${url(params)}`);
  /** @type {SSEStream<T> | null} */
  let stream = null;
  source.onmessage = (event) => {
    const data = event.data;
    if (resTy == "text") {
      stream?.({ type: "message", data });
    } else if (resTy == "json") {
      stream?.({ type: "message", data: JSON.parse(data) });
    } else {
      throw new Error(`Unknown response type: ${resTy}`);
    }
  };
  source.onopen = (event) => {
    stream?.({ type: "open", event });
  };
  source.onerror = (event) => {
    stream?.({ type: "error", event });
  };
  return {
    cancel: () => source.close(),
    listen: (newStream) => (stream = newStream),
  };
};
/**
 * @typedef {Object} ApiOptions
 * @property {typeof fetch} [fetch]
 * @property {string} [apiBase]
 * @property {Record<string, string>} [headers]
 */
/** @typedef {"none" | "json"} RequestType */
/** @typedef {"none" | "text" | "json"} ResponseType */
/** @typedef {"DELETE" | "GET" | "PUT" | "POST" | "HEAD" | "TRACE" | "PATCH"} Method */
/**
 * @typedef {(
 *   event:
 *     | { type: "message"; data: T }
 *     | {
 *         type: "open";
 *         event: Event;
 *       }
 *     | {
 *         type: "error";
 *         event: Event;
 *       }
 * ) => void} SSEStream
 * @template T
 */


  /**
   * @typedef {{ files: Record<string, string> }} slang_ui.SampleFiles */
  /**
   * @typedef {{ file: string, pos: slang_ui.monaco.MonacoPosition }} slang_ui.HoverParams */
  /**
   * @typedef {{ markers: slang_ui.monaco.MarkerData[], analysis_errored: boolean, message: ([(string | null), slang_ui.Color] | null) }} slang_ui.AnalyzeResult */
  /** @typedef {"Alive"} slang_ui.Heartbeat */
  export const HEARTBEAT = /** @type {slang_ui.Heartbeat[]} */ (["Alive"]);
  /**
   * @typedef {{ file: string }} slang_ui.AnalyzeParams */
  /**
   * @typedef {{ span: slang_ui.monaco.MonacoSpan, contents: string[] }} slang_ui.HoverResult */
  /** @typedef {"Red" | "Green" | "Blue"} slang_ui.Color */
  export const COLOR = /** @type {slang_ui.Color[]} */ (["Red", "Green", "Blue"]);
  
    /**
     * @typedef {{ lineNumber: number, column: number }} slang_ui.monaco.MonacoPosition */
    /**
     * @typedef {{ relatedInformation: (slang_ui.monaco.RelatedInformation[] | null), tags: (slang_ui.monaco.MarkerTag[] | null), severity: slang_ui.monaco.MarkerSeverity, message: string, span: slang_ui.monaco.MonacoSpan }} slang_ui.monaco.MarkerData */
    /**
     * @typedef {{ startLineNumber: number, startColumn: number, endLineNumber: number, endColumn: number }} slang_ui.monaco.MonacoSpan */
    /** @typedef {"Hint" | "Info" | "Warning" | "Error"} slang_ui.monaco.MarkerSeverity */
    export const MARKER_SEVERITY = /** @type {slang_ui.monaco.MarkerSeverity[]} */ (["Hint", "Info", "Warning", "Error"]);
    /**
     * @typedef {{ resource: string, message: string, span: slang_ui.monaco.MonacoSpan }} slang_ui.monaco.RelatedInformation */
    /** @typedef {"Unnecessary" | "Deprecated"} slang_ui.monaco.MarkerTag */
    export const MARKER_TAG = /** @type {slang_ui.monaco.MarkerTag[]} */ (["Unnecessary", "Deprecated"]);
  

export const api = {
  sampleFiles: /** @type {ReturnType<typeof request<Record<string, never>, slang_ui.SampleFiles>>} */ (
    request("none", "GET", "/sample-files", "json")
  ),
  heartbeat: /** @type {ReturnType<typeof sse<[], slang_ui.Heartbeat>>} */ (
    sse(() => `/heartbeat`, "json")
  ),
  analyze: /** @type {ReturnType<typeof request<slang_ui.AnalyzeParams, slang_ui.AnalyzeResult>>} */ (
    request("json", "POST", "/analyze", "json")
  ),
  hover: /** @type {ReturnType<typeof request<slang_ui.HoverParams, (slang_ui.HoverResult | null)>>} */ (
    request("json", "POST", "/hover", "json")
  ),
};
