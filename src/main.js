// Clase del Visualizador del Sistema
class SystemVisualizer {
  constructor() {
    this.processesContainer = document.getElementById('processesContainer');
    this.messagesContainer = document.getElementById('messagesContainer');
  }

  renderState(state) {
    console.log('Renderizando estado:', state);
    this.renderProcesses(state.processes);
    this.renderMessages(state.messages);
  }

  renderProcesses(processes) {
    console.log('Renderizando procesos:', processes);
    this.processesContainer.innerHTML = '';
    
    if (!processes || processes.length === 0) {
      this.processesContainer.innerHTML = '<div class="no-processes">No hay procesos para mostrar</div>';
      return;
    }
    
    processes.forEach(process => {
      const processCard = this.createProcessCard(process);
      this.processesContainer.appendChild(processCard);
    });
  }

  createProcessCard(process) {
    const card = document.createElement('div');
    card.className = 'process-card';
    card.innerHTML = `
      <div class="process-header">
        <h3 class="process-id">${process.id}</h3>
        <span class="process-state">${process.state}</span>
      </div>
      
      <div class="process-section">
        <h4>Historial</h4>
        <div class="history-list">
          ${process.history.map(action => `
            <div class="history-item ${this.getActionType(action)}">
              ${this.formatAction(action)}
            </div>
          `).join('')}
        </div>
      </div>
      
      <div class="process-section">
        <h4>Cola</h4>
        <div class="queue-list">
          ${process.queue.map(action => `
            <div class="queue-item ${this.getActionType(action)}">
              ${this.formatAction(action)}
            </div>
          `).join('')}
        </div>
      </div>
    `;
    return card;
  }

  renderMessages(messages) {
    this.messagesContainer.innerHTML = '';
    
    if (messages.length === 0) {
      this.messagesContainer.innerHTML = '<div class="no-messages">No hay mensajes en tránsito</div>';
      return;
    }

    messages.forEach(message => {
      const messageCard = this.createMessageCard(message);
      this.messagesContainer.appendChild(messageCard);
    });
  }

  createMessageCard(message) {
    const card = document.createElement('div');
    card.className = 'message-card';
    card.innerHTML = `
      <div class="message-flow">
        <span class="sender">${message.from}</span>
        <div class="arrow">→</div>
        <span class="receiver">${message.to}</span>
      </div>
      <div class="message-tag">${message.tag}</div>
    `;
    return card;
  }

  getActionType(action) {
    if (action.includes('send(')) return 'action-send';
    if (action.includes('receive(')) return 'action-receive';
    if (action.includes('spawn(')) return 'action-spawn';
    if (action.includes('check(')) return 'action-checkpoint';
    if (action.includes('commit(')) return 'action-commit';
    if (action.includes('rollback(')) return 'action-rollback';
    if (action.includes('spawned_by(')) return 'action-spawned';
    return 'action-default';
  }

  formatAction(action) {
    // Clean up the action string for better display
    return action
      .replace(/([a-z]+)\(/g, '<strong>$1</strong>(')
      .replace(/\)/g, ')');
  }

  clear() {
    this.processesContainer.innerHTML = '';
    this.messagesContainer.innerHTML = '<div class="no-messages">No hay mensajes en tránsito</div>';
  }
}

// Clase Principal de la Aplicación
class App {
  constructor() {
    this.visualizer = new SystemVisualizer();
    this.executor = new Executor();
    this.currentStep = 0;
    this.currentState = null;
    this.initializeEventListeners();
    console.log('Aplicación inicializada correctamente');
  }

  initializeEventListeners() {
    document.getElementById('loadBtn').addEventListener('click', () => this.loadSystemStates());
    document.getElementById('stepBtn').addEventListener('click', () => this.nextStep());
    document.getElementById('resetBtn').addEventListener('click', () => this.reset());
    console.log('Listeners de eventos inicializados');
  }

  async loadSystemStates() {
    console.log('Cargando estado inicial del sistema...');
    
    this.logMessage('Cargando estado inicial...', 'info');
    const result = await this.executor.initializeFromTraces(['p1.txt', 'p2.txt', 'p3.txt', 'p4.txt']);
    
    if (result.success) {
      this.currentState = result.state;
      this.currentStep = 0;
      this.updateUI();
      this.logMessage('Estado inicial cargado correctamente', 'success');
      document.getElementById('stepBtn').disabled = false;
    } else {
      this.logMessage(`Error al cargar estado inicial: ${result.error}`, 'error');
    }
  }

  async nextStep() {
    this.logMessage('Ejecutando siguiente paso...', 'info');
    const result = await this.executor.executeNextStep();
    
    if (result.success) {
      this.currentState = result.newState;
      this.currentStep = result.step;
      
      this.visualizer.renderState(result.newState);
      document.getElementById('stepCounter').textContent = `Paso: ${result.step}`;
      this.logMessage(`Ejecutado: ${result.executedAction}`, 'action');
    } else {
      if (result.isFinished) {
        this.logMessage('Ejecución completada', 'success');
        document.getElementById('stepBtn').disabled = true;
      } else {
        this.logMessage(`Error al ejecutar paso: ${result.error}`, 'error');
      }
    }
  }

  async reset() {
    this.visualizer.clear();
    document.getElementById('stepCounter').textContent = 'Paso: 0';
    document.getElementById('stepBtn').disabled = true;
    document.getElementById('logContainer').innerHTML = '';
    
    await this.executor.reset();
    this.currentState = null;
    this.currentStep = 0;
    
    this.logMessage('Sistema reiniciado', 'info');
  }

  updateUI() {
    if (this.currentState) {
      this.visualizer.renderState(this.currentState);
      document.getElementById('stepCounter').textContent = `Paso: ${this.currentStep}`;
    }
  }

  logMessage(message, type = 'info') {
    const logContainer = document.getElementById('logContainer');
    const logEntry = document.createElement('div');
    logEntry.className = `log-entry log-${type}`;
    logEntry.innerHTML = `
      <span class="timestamp">[${new Date().toLocaleTimeString()}]</span>
      <span class="message">${message}</span>
    `;
    logContainer.appendChild(logEntry);
    logContainer.scrollTop = logContainer.scrollHeight;
  }
}

// Inicializar la aplicación cuando se carga la página
document.addEventListener('DOMContentLoaded', function() {
  console.log('DOM cargado, inicializando aplicación...');
  new App();
});