describe('Sign-in tests', () => {
  it('Gets the login page on first access', () => {
    cy.visit('http://localhost:8888/')
    cy.location('pathname').should('eq', '/sign-in')
    cy.get('input[id="Name"]').type('Yarrow')
    cy.get('button').click()
    cy.location('pathname').should('eq', '/')
    cy.contains('Yarrow')
  })
})
