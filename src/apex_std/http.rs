//! HTTP module for Apex
//!
//! HTTP client for making requests:
//! - Client: HTTP client
//! - Request: HTTP request builder
//! - Response: HTTP response
//! - Method: HTTP methods (GET, POST, etc.)

use std::collections::HashMap;
use std::time::Duration;

/// HTTP methods
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Method {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
    TRACE,
    CONNECT,
}

impl Method {
    /// Convert to string
    pub fn as_str(&self) -> &'static str {
        match self {
            Method::GET => "GET",
            Method::POST => "POST",
            Method::PUT => "PUT",
            Method::DELETE => "DELETE",
            Method::PATCH => "PATCH",
            Method::HEAD => "HEAD",
            Method::OPTIONS => "OPTIONS",
            Method::TRACE => "TRACE",
            Method::CONNECT => "CONNECT",
        }
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// HTTP status codes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StatusCode(u16);

impl StatusCode {
    /// Create a new status code
    pub fn new(code: u16) -> Self {
        Self(code)
    }

    /// Get the status code number
    pub fn as_u16(&self) -> u16 {
        self.0
    }

    /// Check if informational (1xx)
    pub fn is_informational(&self) -> bool {
        self.0 >= 100 && self.0 < 200
    }

    /// Check if successful (2xx)
    pub fn is_success(&self) -> bool {
        self.0 >= 200 && self.0 < 300
    }

    /// Check if redirection (3xx)
    pub fn is_redirection(&self) -> bool {
        self.0 >= 300 && self.0 < 400
    }

    /// Check if client error (4xx)
    pub fn is_client_error(&self) -> bool {
        self.0 >= 400 && self.0 < 500
    }

    /// Check if server error (5xx)
    pub fn is_server_error(&self) -> bool {
        self.0 >= 500 && self.0 < 600
    }

    /// Check if error (4xx or 5xx)
    pub fn is_error(&self) -> bool {
        self.is_client_error() || self.is_server_error()
    }

    /// Get canonical reason phrase
    pub fn canonical_reason(&self) -> Option<&'static str> {
        match self.0 {
            100 => Some("Continue"),
            101 => Some("Switching Protocols"),
            200 => Some("OK"),
            201 => Some("Created"),
            202 => Some("Accepted"),
            204 => Some("No Content"),
            301 => Some("Moved Permanently"),
            302 => Some("Found"),
            304 => Some("Not Modified"),
            400 => Some("Bad Request"),
            401 => Some("Unauthorized"),
            403 => Some("Forbidden"),
            404 => Some("Not Found"),
            405 => Some("Method Not Allowed"),
            500 => Some("Internal Server Error"),
            502 => Some("Bad Gateway"),
            503 => Some("Service Unavailable"),
            _ => None,
        }
    }
}

impl std::fmt::Display for StatusCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.canonical_reason() {
            Some(reason) => write!(f, "{} {}", self.0, reason),
            None => write!(f, "{}", self.0),
        }
    }
}

/// HTTP headers
#[derive(Debug, Clone, Default)]
pub struct Headers {
    inner: HashMap<String, Vec<String>>,
}

impl Headers {
    /// Create empty headers
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    /// Insert a header
    pub fn insert(&mut self, name: impl Into<String>, value: impl Into<String>) {
        let name = name.into().to_lowercase();
        let value = value.into();
        self.inner.entry(name).or_default().push(value);
    }

    /// Set a header (replaces existing)
    pub fn set(&mut self, name: impl Into<String>, value: impl Into<String>) {
        let name = name.into().to_lowercase();
        self.inner.insert(name, vec![value.into()]);
    }

    /// Get a header
    pub fn get(&self, name: &str) -> Option<&str> {
        self.inner
            .get(&name.to_lowercase())
            .and_then(|v| v.first())
            .map(|s| s.as_str())
    }

    /// Get all values for a header
    pub fn get_all(&self, name: &str) -> Option<&[String]> {
        self.inner.get(&name.to_lowercase()).map(|v| v.as_slice())
    }

    /// Check if header exists
    pub fn contains(&self, name: &str) -> bool {
        self.inner.contains_key(&name.to_lowercase())
    }

    /// Remove a header
    pub fn remove(&mut self, name: &str) -> Option<Vec<String>> {
        self.inner.remove(&name.to_lowercase())
    }

    /// Get all header names
    pub fn names(&self) -> impl Iterator<Item = &str> {
        self.inner.keys().map(|s| s.as_str())
    }

    /// Get content type
    pub fn content_type(&self) -> Option<&str> {
        self.get("content-type")
    }

    /// Get content length
    pub fn content_length(&self) -> Option<usize> {
        self.get("content-length")?.parse().ok()
    }
}

/// HTTP request
#[derive(Debug, Clone)]
pub struct Request {
    method: Method,
    url: String,
    headers: Headers,
    body: Option<Vec<u8>>,
    timeout: Option<Duration>,
}

impl Request {
    /// Create a new request
    pub fn new(method: Method, url: impl Into<String>) -> Self {
        Self {
            method,
            url: url.into(),
            headers: Headers::new(),
            body: None,
            timeout: None,
        }
    }

    /// Create a GET request
    pub fn get(url: impl Into<String>) -> Self {
        Self::new(Method::GET, url)
    }

    /// Create a POST request
    pub fn post(url: impl Into<String>) -> Self {
        Self::new(Method::POST, url)
    }

    /// Create a PUT request
    pub fn put(url: impl Into<String>) -> Self {
        Self::new(Method::PUT, url)
    }

    /// Create a DELETE request
    pub fn delete(url: impl Into<String>) -> Self {
        Self::new(Method::DELETE, url)
    }

    /// Create a PATCH request
    pub fn patch(url: impl Into<String>) -> Self {
        Self::new(Method::PATCH, url)
    }

    /// Set header
    pub fn header(mut self, name: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(name, value);
        self
    }

    /// Set body
    pub fn body(mut self, body: impl Into<Vec<u8>>) -> Self {
        self.body = Some(body.into());
        self
    }

    /// Set JSON body
    pub fn json(mut self, json: &str) -> Self {
        self.headers.set("content-type", "application/json");
        self.body = Some(json.as_bytes().to_vec());
        self
    }

    /// Set form body
    pub fn form(mut self, form: &[(&str, &str)]) -> Self {
        self.headers
            .set("content-type", "application/x-www-form-urlencoded");
        let body: Vec<String> = form
            .iter()
            .map(|(k, v)| format!("{}={}", url_encode(k), url_encode(v)))
            .collect();
        self.body = Some(body.join("&").into_bytes());
        self
    }

    /// Set timeout
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Get method
    pub fn method(&self) -> &Method {
        &self.method
    }

    /// Get URL
    pub fn url(&self) -> &str {
        &self.url
    }

    /// Get headers
    pub fn headers(&self) -> &Headers {
        &self.headers
    }

    /// Get mutable headers
    pub fn headers_mut(&mut self) -> &mut Headers {
        &mut self.headers
    }

    /// Get body
    pub fn body(&self) -> Option<&[u8]> {
        self.body.as_deref()
    }

    /// Get timeout
    pub fn timeout(&self) -> Option<Duration> {
        self.timeout
    }
}

/// HTTP response
#[derive(Debug, Clone)]
pub struct Response {
    status: StatusCode,
    headers: Headers,
    body: Vec<u8>,
}

impl Response {
    /// Create a new response
    pub fn new(status: StatusCode) -> Self {
        Self {
            status,
            headers: Headers::new(),
            body: Vec::new(),
        }
    }

    /// Get status code
    pub fn status(&self) -> StatusCode {
        self.status
    }

    /// Get headers
    pub fn headers(&self) -> &Headers {
        &self.headers
    }

    /// Get body as bytes
    pub fn body(&self) -> &[u8] {
        &self.body
    }

    /// Get body as string
    pub fn text(&self) -> Result<String, std::str::Utf8Error> {
        std::str::from_utf8(&self.body).map(|s| s.to_string())
    }

    /// Check if successful
    pub fn is_success(&self) -> bool {
        self.status.is_success()
    }

    /// Check if error
    pub fn is_error(&self) -> bool {
        self.status.is_error()
    }

    /// Error for status
    pub fn error_for_status(self) -> Result<Self, HttpError> {
        if self.is_error() {
            Err(HttpError::StatusError(self.status))
        } else {
            Ok(self)
        }
    }
}

/// HTTP error
#[derive(Debug, Clone)]
pub enum HttpError {
    NetworkError(String),
    TimeoutError,
    StatusError(StatusCode),
    InvalidUrl(String),
    ParseError(String),
}

impl std::fmt::Display for HttpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HttpError::NetworkError(msg) => write!(f, "Network error: {}", msg),
            HttpError::TimeoutError => write!(f, "Request timed out"),
            HttpError::StatusError(status) => write!(f, "HTTP error: {}", status),
            HttpError::InvalidUrl(url) => write!(f, "Invalid URL: {}", url),
            HttpError::ParseError(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for HttpError {}

/// HTTP client
#[derive(Debug, Clone)]
pub struct Client {
    base_url: Option<String>,
    default_headers: Headers,
    timeout: Option<Duration>,
    follow_redirects: bool,
    max_redirects: usize,
}

impl Client {
    /// Create a new client
    pub fn new() -> Self {
        Self {
            base_url: None,
            default_headers: Headers::new(),
            timeout: Some(Duration::from_secs(30)),
            follow_redirects: true,
            max_redirects: 10,
        }
    }

    /// Set base URL
    pub fn base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = Some(url.into());
        self
    }

    /// Set default header
    pub fn default_header(mut self, name: impl Into<String>, value: impl Into<String>) -> Self {
        self.default_headers.insert(name, value);
        self
    }

    /// Set timeout
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Set follow redirects
    pub fn follow_redirects(mut self, follow: bool) -> Self {
        self.follow_redirects = follow;
        self
    }

    /// Execute a request
    pub fn execute(&self, request: Request) -> Result<Response, HttpError> {
        // This is a placeholder implementation
        // In a real implementation, this would make an actual HTTP request
        
        // Build full URL
        let url = if let Some(base) = &self.base_url {
            if request.url.starts_with("http") {
                request.url.clone()
            } else {
                format!("{}{}", base.trim_end_matches('/'), request.url)
            }
        } else {
            request.url.clone()
        };

        // Validate URL
        if !url.starts_with("http://") && !url.starts_with("https://") {
            return Err(HttpError::InvalidUrl(url));
        }

        // For now, return a mock successful response
        // TODO: Implement actual HTTP request using a library like reqwest or hyper
        
        let mut response = Response::new(StatusCode::new(200));
        response.headers.set("content-type", "application/json");
        response.body = br#"{"status": "ok"}"#.to_vec();
        
        Ok(response)
    }

    /// Make a GET request
    pub fn get(&self, url: impl Into<String>) -> Result<Response, HttpError> {
        self.execute(Request::get(url))
    }

    /// Make a POST request
    pub fn post(&self, url: impl Into<String>) -> Result<Response, HttpError> {
        self.execute(Request::post(url))
    }

    /// Make a PUT request
    pub fn put(&self, url: impl Into<String>) -> Result<Response, HttpError> {
        self.execute(Request::put(url))
    }

    /// Make a DELETE request
    pub fn delete(&self, url: impl Into<String>) -> Result<Response, HttpError> {
        self.execute(Request::delete(url))
    }

    /// Make a PATCH request
    pub fn patch(&self, url: impl Into<String>) -> Result<Response, HttpError> {
        self.execute(Request::patch(url))
    }
}

impl Default for Client {
    fn default() -> Self {
        Self::new()
    }
}

/// URL encode a string
fn url_encode(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            'A'..='Z' | 'a'..='z' | '0'..='9' | '-' | '_' | '.' | '~' => result.push(c),
            ' ' => result.push('+'),
            c => {
                for b in c.encode_utf8(&mut [0; 4]).bytes() {
                    result.push('%');
                    result.push_str(&format!("{:02X}", b));
                }
            }
        }
    }
    result
}

/// Simple HTTP GET request
pub fn get(url: impl Into<String>) -> Result<Response, HttpError> {
    Client::new().get(url)
}

/// Simple HTTP POST request
pub fn post(url: impl Into<String>) -> Result<Response, HttpError> {
    Client::new().post(url)
}

/// Simple HTTP PUT request
pub fn put(url: impl Into<String>) -> Result<Response, HttpError> {
    Client::new().put(url)
}

/// Simple HTTP DELETE request
pub fn delete(url: impl Into<String>) -> Result<Response, HttpError> {
    Client::new().delete(url)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_method() {
        assert_eq!(Method::GET.as_str(), "GET");
        assert_eq!(Method::POST.as_str(), "POST");
        assert_eq!(Method::PUT.to_string(), "PUT");
    }

    #[test]
    fn test_status_code() {
        let ok = StatusCode::new(200);
        assert!(ok.is_success());
        assert!(!ok.is_error());
        assert_eq!(ok.canonical_reason(), Some("OK"));

        let not_found = StatusCode::new(404);
        assert!(!not_found.is_success());
        assert!(not_found.is_error());
        assert!(not_found.is_client_error());

        let server_error = StatusCode::new(500);
        assert!(server_error.is_server_error());
    }

    #[test]
    fn test_headers() {
        let mut headers = Headers::new();
        headers.insert("content-type", "application/json");
        headers.insert("authorization", "Bearer token");

        assert_eq!(headers.get("content-type"), Some("application/json"));
        assert_eq!(headers.get("Content-Type"), Some("application/json"));
        assert!(headers.contains("authorization"));
    }

    #[test]
    fn test_request_builder() {
        let req = Request::get("https://example.com")
            .header("accept", "application/json")
            .timeout(Duration::from_secs(10));

        assert_eq!(req.method(), &Method::GET);
        assert_eq!(req.url(), "https://example.com");
        assert_eq!(req.headers().get("accept"), Some("application/json"));
        assert_eq!(req.timeout(), Some(Duration::from_secs(10)));
    }

    #[test]
    fn test_request_with_json() {
        let req = Request::post("https://api.example.com/users")
            .json(r#"{"name": "John"}"#);

        assert_eq!(req.headers().content_type(), Some("application/json"));
        assert!(req.body().is_some());
    }

    #[test]
    fn test_url_encode() {
        assert_eq!(url_encode("hello world"), "hello+world");
        assert_eq!(url_encode("a@b.c"), "a%40b.c");
    }
}
