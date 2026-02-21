//! Apex Project Configuration
//!
//! Supports multi-file projects with apex.toml configuration

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Project configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    /// Project name
    pub name: String,
    /// Project version
    pub version: String,
    /// Entry point file (contains main function)
    pub entry: String,
    /// Source files to compile (relative to project root)
    pub files: Vec<String>,
    /// Output binary name
    #[serde(default = "default_output")]
    pub output: String,
    /// Additional compiler flags
    #[serde(default)]
    pub flags: Vec<String>,
    /// Dependencies (future use)
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    /// Optimization level
    #[serde(default = "default_opt_level")]
    pub opt_level: String,
    /// Target triple (optional)
    #[serde(default)]
    pub target: Option<String>,
}

fn default_output() -> String {
    "main".to_string()
}

fn default_opt_level() -> String {
    "3".to_string()
}

impl Default for ProjectConfig {
    fn default() -> Self {
        Self {
            name: "untitled".to_string(),
            version: "0.1.0".to_string(),
            entry: "main.apex".to_string(),
            files: vec!["main.apex".to_string()],
            output: default_output(),
            flags: vec![],
            dependencies: HashMap::new(),
            opt_level: default_opt_level(),
            target: None,
        }
    }
}

impl ProjectConfig {
    /// Load project config from apex.toml
    pub fn load(path: &Path) -> Result<Self, String> {
        let content = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read project file: {}", e))?;
        
        let config: ProjectConfig = toml::from_str(&content)
            .map_err(|e| format!("Failed to parse project file: {}", e))?;
        
        Ok(config)
    }
    
    /// Save project config to apex.toml
    pub fn save(&self, path: &Path) -> Result<(), String> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| format!("Failed to serialize project: {}", e))?;
        
        fs::write(path, content)
            .map_err(|e| format!("Failed to write project file: {}", e))?;
        
        Ok(())
    }
    
    /// Create default project config
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            version: "0.1.0".to_string(),
            entry: "main.apex".to_string(),
            files: vec!["main.apex".to_string()],
            output: name.to_string(),
            flags: vec![],
            dependencies: HashMap::new(),
            opt_level: "3".to_string(),
            target: None,
        }
    }
    
    /// Get all source files as absolute paths
    pub fn get_source_files(&self, project_root: &Path) -> Vec<PathBuf> {
        self.files
            .iter()
            .map(|f| project_root.join(f))
            .collect()
    }
    
    /// Get entry point as absolute path
    pub fn get_entry_path(&self, project_root: &Path) -> PathBuf {
        project_root.join(&self.entry)
    }
    
    /// Validate project configuration
    pub fn validate(&self, project_root: &Path) -> Result<(), String> {
        // Check entry point exists
        let entry_path = self.get_entry_path(project_root);
        if !entry_path.exists() {
            return Err(format!(
                "Entry point '{}' not found at '{}'",
                self.entry,
                entry_path.display()
            ));
        }
        
        // Check all source files exist
        for file in &self.files {
            let file_path = project_root.join(file);
            if !file_path.exists() {
                return Err(format!(
                    "Source file '{}' not found at '{}'",
                    file,
                    file_path.display()
                ));
            }
        }
        
        // Check entry is in files list
        if !self.files.contains(&self.entry) {
            return Err(format!(
                "Entry point '{}' must be listed in files",
                self.entry
            ));
        }
        
        Ok(())
    }
}

/// Find project root by looking for apex.toml
pub fn find_project_root(start_dir: &Path) -> Option<PathBuf> {
    let mut current = Some(start_dir);
    
    while let Some(dir) = current {
        let config_path = dir.join("apex.toml");
        if config_path.exists() {
            return Some(dir.to_path_buf());
        }
        
        current = dir.parent();
    }
    
    None
}

/// Check if path is inside a project
pub fn is_in_project(path: &Path) -> bool {
    find_project_root(path).is_some()
}
