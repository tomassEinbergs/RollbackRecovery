// Parser de archivos
class FileParser {
  constructor() {
    // Ruta base para archivos de trazas
    this.tracesPath = 'traces/';
  }

  // Parsear un solo archivo de trazas
  async parseTraceFile(filename) {
    try {
      const response = await fetch(`${this.tracesPath}${filename}`);
      if (!response.ok) {
        throw new Error(`Error al cargar archivo de trazas: ${filename}`);
      }
      
      const fileContent = await response.text();
      const traceData = this.parseFileContent(fileContent);
      
      const actions = [];
      for (const line of traceData) {
        const action = this.convertAction(line);
        if (action) {
          actions.push(action);
        }
      }
      
      console.log(`Cargado ${filename}:`, actions);
      return actions;
    } catch (error) {
      console.error(`Error leyendo archivo de trazas ${filename}:`, error);
      return [];
    }
  }

  // Parsear contenido del archivo en líneas de trazas individuales
  parseFileContent(content) {
    return content
      .split('\n')
      .map(line => line.trim())
      .filter(line => line && !line.startsWith('%')); // Remover líneas vacías y comentarios
  }

  // Convertir acción del formato de archivo al formato interno
  convertAction(actionStr) {
    // Remover llaves y parsear
    const cleaned = actionStr.replace(/[{}]/g, '').trim();
    const parts = cleaned.split(',').map(p => p.trim());

    switch (parts[0]) {
      case 'send':
        return `send(${parts[1]},${parts[2]})`;
      case 'receive':
        return `receive(${parts[1]})`;
      case 'check':
        return `check(${parts[1]})`;
      case 'commit':
        return `commit(${parts[1]})`;
      case 'rollback':
        return `rollback(${parts[1]})`;
      case 'spawn':
        return `spawn(${parts[1]})`;
      default:
        return null;
    }
  }
}

// Hacerlo disponible globalmente
window.FileParser = FileParser;